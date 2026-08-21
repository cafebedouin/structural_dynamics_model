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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: tcp_ip_interpretation__prioritization_reading
 *   human_readable: TCP/IP Interpretation: Prioritization for Network Management
 *   domain: Technology Governance / Internet Policy / Telecommunications Law
 *
 * SUMMARY:
 *   This constraint represents the 'prioritization reading' of TCP/IP, which
 *   interprets the protocol stack as permitting Internet Service Providers
 *   (ISPs) to implement differentiated service quality, including paid 'fast
 *   lanes.' This reading is often justified as a necessary mechanism for
 *   network management and to incentivize infrastructure investment. It
 *   stands in direct contrast to the 'neutrality reading' which emphasizes
 *   non-discrimination, and coexists with the 'zero-rating reading' which
 *   allows selective exemptions for sponsored content. The authored metrics
 *   reflect the substantial extraction and suppression inherent in this
 *   interpretation's operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__prioritization_reading, 0.78).
domain_priors:suppression_score(tcp_ip_interpretation__prioritization_reading, 0.85).
domain_priors:theater_ratio(tcp_ip_interpretation__prioritization_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__prioritization_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__prioritization_reading, "TCP/IP Interpretation: Prioritization for Network Management").
narrative_ontology:topic_domain(tcp_ip_interpretation__prioritization_reading, "Technology Governance / Internet Policy / Telecommunications Law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__prioritization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__prioritization_reading, '7b657f4a-73dd-4811-bdd3-477841704a45').
narrative_ontology:cs_kernel_codification('7b657f4a-73dd-4811-bdd3-477841704a45', fixed_text).
narrative_ontology:cs_authority_grounding('7b657f4a-73dd-4811-bdd3-477841704a45', extraction).
narrative_ontology:cs_interpretation_layer_present('7b657f4a-73dd-4811-bdd3-477841704a45').
narrative_ontology:cs_reading_relation('7b657f4a-73dd-4811-bdd3-477841704a45', tcp_ip_interpretation__neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('7b657f4a-73dd-4811-bdd3-477841704a45', tcp_ip_interpretation__zero_rating_reading, coexists_with).
narrative_ontology:cs_axiom('7b657f4a-73dd-4811-bdd3-477841704a45', foundational, network_investment_incentive).
narrative_ontology:cs_axiom_status(network_investment_incentive, holdable).
narrative_ontology:cs_axiom_grounding('7b657f4a-73dd-4811-bdd3-477841704a45', network_investment_incentive, empirically_contingent).
narrative_ontology:cs_axiom('7b657f4a-73dd-4811-bdd3-477841704a45', foundational, efficient_resource_allocation).
narrative_ontology:cs_axiom_status(efficient_resource_allocation, holdable).
narrative_ontology:cs_axiom_grounding('7b657f4a-73dd-4811-bdd3-477841704a45', efficient_resource_allocation, instrumental).
narrative_ontology:cs_reference_frame('7b657f4a-73dd-4811-bdd3-477841704a45', market_driven_network_management).
narrative_ontology:cs_drift_state('7b657f4a-73dd-4811-bdd3-477841704a45', contemporary_regulatory_environment, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7b657f4a-73dd-4811-bdd3-477841704a45', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, internet_service_providers).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, large_content_providers_paying_for_fast_lanes).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, edge_service_startups).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, net_neutrality_advocates).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, consumers_of_non_prioritized_content).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, large_content_providers_paying_for_fast_lanes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and implement network prioritization schemes, charging content providers for faster lanes. They claim this is essential for network management and incentivizes infrastructure investment. They directly benefit from the revenue generated.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, internet_service_providers, agenda_setter,
    institutional, generational, arbitrage, national).

% Pay ISPs for prioritized access to ensure their content reaches users quickly and reliably. While they bear a cost, they benefit from a competitive advantage over rivals who cannot afford prioritization, securing market share and user experience.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, large_content_providers_paying_for_fast_lanes, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, large_content_providers_paying_for_fast_lanes, payer).

% Cannot afford to pay for fast lanes, putting them at a significant disadvantage. Their services may be slower or less reliable, hindering growth and user adoption. Their ability to compete is severely constrained by the prioritization regime.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, edge_service_startups, payer,
    powerless, immediate, trapped, global).

% Actively campaign against network prioritization, arguing it violates the open internet principle. They are often excluded from direct policy-making but exert pressure through public discourse, lobbying, and legal challenges.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, net_neutrality_advocates, excluded,
    organized, generational, analytical, national).

% Experience slower speeds or buffering for content from services that cannot afford prioritization. They indirectly pay for fast lanes through higher subscription fees for prioritized services, or suffer degraded quality for others. Their choices are limited by ISP policies.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, consumers_of_non_prioritized_content, payer,
    moderate, biographical, constrained, global).

% Interpret telecommunications law and TCP/IP protocols to determine the legality and scope of network prioritization. Their decisions directly shape the constraint, sometimes enabling it, sometimes restricting it, based on prevailing political and economic pressures.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, telecom_regulators, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, telecom_regulators, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__prioritization_reading, internet_service_providers).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__prioritization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To manage network congestion and allocate bandwidth efficiently, ensuring critical or paid-for services receive necessary quality of service, and to incentivize investment in network infrastructure by creating new revenue streams for ISPs.
% TRANSFER_FUNCTION: Moves revenue from content providers (and indirectly from consumers) to Internet Service Providers in exchange for prioritized data delivery, ensuring certain traffic flows faster than others.
% ABSENT_VOICES: Small edge providers, independent developers, and consumer advocacy groups who cannot afford or do not benefit from prioritization are often marginalized in policy debates, despite bearing significant costs. They would argue for a truly open and non-discriminatory internet.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished overnight, ISPs would lose a significant revenue stream, potentially impacting their investment models. All internet traffic would be treated equally, leading to a reorganization of the digital economy, potentially benefiting smaller content providers and altering consumer expectations for service quality.
% FOUNDING_PROBLEM: The internet faced challenges of network congestion and a perceived lack of incentive for ISPs to invest in upgrading infrastructure to handle ever-increasing traffic demands.
% FOUNDING_PROBLEM_CORROBORATION: Internet Service Providers and some investors attest that the problem of investment incentive and congestion management is still live. Net neutrality advocates and many edge providers contest this, arguing that the problem is largely a pretext for rent-seeking, citing independent economic analyses and historical ISP profitability.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__prioritization_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__prioritization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__prioritization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(tcp_ip_interpretation__prioritization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__prioritization_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.78) is high because ISPs leverage their control over last-mile access to charge content providers for a service (prioritized delivery) that is arguably a core function of an open internet. Suppression (0.85) is also high, as ISPs actively block or degrade non-paying traffic, and regulatory frameworks often support this enforcement. The theater ratio (0.4) reflects that while some technical network management is involved, a significant portion of the justification serves commercial interests rather than purely technical necessity. The increasing trend in extractiveness and suppression over time reflects the hardening of this interpretation in various regulatory environments.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of ISPs, this interpretation is a necessary and efficient way to manage networks and fund investment. From the perspective of edge providers and net neutrality advocates, it is a mechanism for rent extraction that undermines the internet's open nature. The engine's classification will highlight this divergence by computing different effective extraction values for each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Internet Service Providers are clear beneficiaries and agenda-setters, collecting revenue and shaping policy. Large content providers who pay for fast lanes also benefit from a competitive advantage, even as they bear costs. Edge service startups and consumers of non-prioritized content are the primary targets, experiencing degraded service and constrained market access. Net neutrality advocates are excluded from the direct enforcement mechanism but resist through other channels.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (network management, investment incentive) is contested. While some technical justification exists, the high extractiveness and suppression suggest that the coordination function is substantially overshadowed by rent-seeking. The 'tangled_rope' classification captures this hybrid nature, preventing it from being mislabeled as pure coordination (Rope) or pure extraction (Snare) without acknowledging its dual function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    investment_incentive_efficacy,
    'Does network prioritization genuinely incentivize new, otherwise unfunded, infrastructure investment, or does it primarily reallocate existing revenue streams and increase ISP profits?',
    'Longitudinal economic studies comparing ISP investment patterns in jurisdictions with and without prioritization, controlling for other market factors.',
    'If investment is not significantly boosted, the ''investment incentive'' justification for prioritization weakens, increasing the constraint''s effective extractiveness and shifting its classification closer to a Snare. If investment is clearly linked, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investment_incentive_efficacy, empirical, 'Whether prioritization actually drives infrastructure investment.').

omega_variable(
    technical_necessity_vs_commercial_choice,
    'To what extent is network prioritization a technical necessity for efficient network management (e.g., for critical services like telemedicine), versus a commercial choice to create tiered service offerings?',
    'Independent technical audits of network operations and traffic patterns, distinguishing between genuine congestion management for critical services and commercial differentiation of non-critical traffic.',
    'If prioritization is primarily a commercial choice, the ''network management'' coordination story becomes more theatrical, increasing the constraint''s theater_ratio and effective extractiveness. If technically necessary, the coordination function is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_necessity_vs_commercial_choice, empirical, 'Distinguishing technical need from commercial preference in prioritization.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this ''prioritization_reading'' a valid interpretation of TCP/IP''s design principles, or a reinterpretation driven by commercial interests that fundamentally alters the internet''s original architecture?',
    'Historical analysis of TCP/IP design documents, early internet engineering principles (e.g., end-to-end principle), and expert consensus from network architects not affiliated with ISPs.',
    'If deemed a reinterpretation, the legitimacy of the ''prioritization_reading'' is undermined, strengthening the ''neutrality_reading'' and potentially leading to regulatory challenges that could dismantle the constraint. If deemed valid, the constraint''s naturalness claim is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Validity of prioritization as a TCP/IP interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__prioritization_reading, 2000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t2000, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(tcp__tr_t2005, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(tcp__tr_t2010, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(tcp__tr_t2015, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(tcp__tr_t2020, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(tcp__tr_t2024, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(tcp__be_t2000, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(tcp__be_t2005, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(tcp__be_t2010, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(tcp__be_t2015, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2015, 0.72).
narrative_ontology:measurement(tcp__be_t2020, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2020, 0.75).
narrative_ontology:measurement(tcp__be_t2024, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t2000, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(tcp__su_t2005, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(tcp__su_t2010, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(tcp__su_t2015, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2015, 0.78).
narrative_ontology:measurement(tcp__su_t2020, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2020, 0.82).
narrative_ontology:measurement(tcp__su_t2024, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__prioritization_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'tcp_ip_interpretation' kernel, alongside 'neutrality_reading' and 'zero_rating_reading'. Each reading represents a distinct structural constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
