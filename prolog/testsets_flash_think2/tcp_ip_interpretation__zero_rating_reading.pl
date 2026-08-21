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
 *   human_readable: TCP/IP Interpretation: Zero-Rating Reading
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   This constraint represents the 'zero-rating' reading of TCP/IP, where
 *   Internet Service Providers (ISPs) are permitted to exempt specific
 *   content from users' data caps, often in partnership with content
 *   providers. This interpretation contrasts sharply with net neutrality
 *   principles, allowing for differentiated treatment of internet traffic.
 *   The structural delta for this reading is that ISPs are authorized to
 *   partner with content providers for data cap exemptions, incumbent
 *   platforms are advantaged, and competitive entry barriers are raised. This
 *   story instantiates one specific constraint from the broader
 *   'tcp_ip_interpretation' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, 0.7).
domain_priors:suppression_score(tcp_ip_interpretation__zero_rating_reading, 0.75).
domain_priors:theater_ratio(tcp_ip_interpretation__zero_rating_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__zero_rating_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__zero_rating_reading, "TCP/IP Interpretation: Zero-Rating Reading").
narrative_ontology:topic_domain(tcp_ip_interpretation__zero_rating_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__zero_rating_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__zero_rating_reading, 'cea209d6-5ef2-44a3-86be-00d6cfbba6ec').
narrative_ontology:cs_kernel_codification('cea209d6-5ef2-44a3-86be-00d6cfbba6ec', fixed_text).
narrative_ontology:cs_authority_grounding('cea209d6-5ef2-44a3-86be-00d6cfbba6ec', extraction).
narrative_ontology:cs_interpretation_layer_present('cea209d6-5ef2-44a3-86be-00d6cfbba6ec').
narrative_ontology:cs_reading_relation('cea209d6-5ef2-44a3-86be-00d6cfbba6ec', tcp_ip_interpretation__neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('cea209d6-5ef2-44a3-86be-00d6cfbba6ec', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_axiom('cea209d6-5ef2-44a3-86be-00d6cfbba6ec', foundational, network_owner_discretion_axiom).
narrative_ontology:cs_axiom_status(network_owner_discretion_axiom, holdable).
narrative_ontology:cs_axiom_grounding('cea209d6-5ef2-44a3-86be-00d6cfbba6ec', network_owner_discretion_axiom, conventional).
narrative_ontology:cs_axiom('cea209d6-5ef2-44a3-86be-00d6cfbba6ec', secondary, sponsored_content_as_innovation_driver).
narrative_ontology:cs_axiom_status(sponsored_content_as_innovation_driver, holdable).
narrative_ontology:cs_axiom_grounding('cea209d6-5ef2-44a3-86be-00d6cfbba6ec', sponsored_content_as_innovation_driver, instrumental).
narrative_ontology:cs_reference_frame('cea209d6-5ef2-44a3-86be-00d6cfbba6ec', market_differentiation_framework).
narrative_ontology:cs_drift_state('cea209d6-5ef2-44a3-86be-00d6cfbba6ec', contemporary_regulatory_debate, gap(stable, minor, false)).
narrative_ontology:cs_created_at('cea209d6-5ef2-44a3-86be-00d6cfbba6ec', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, internet_service_providers).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, sponsored_content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, internet_users).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, non_sponsored_content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, startups_and_innovators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% ISPs implement zero-rating policies, exempting specific content from data caps. They benefit from new revenue streams from sponsored content providers and increased customer lock-in due to perceived 'free' data offerings. They actively enforce these policies by managing network traffic and billing.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, internet_service_providers, agenda_setter,
    institutional, generational, arbitrage, national).

% These are large content companies (e.g., social media, streaming services) that partner with ISPs to have their traffic zero-rated. They gain a significant competitive advantage and preferential access to users, especially those on limited data plans.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, sponsored_content_providers, beneficiary,
    powerful, biographical, mobile, global).

% Users benefit from accessing certain content without it counting against their data caps. However, they pay through reduced choice, potential overages for non-zero-rated content, and a less open internet. Their exit options are limited by the availability of alternative ISPs.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, internet_users, payer,
    powerless, biographical, constrained, national).

% These are content creators and platforms that cannot or choose not to pay for zero-rating. They face a significant disadvantage, as their content is effectively more 'expensive' for users on data-capped plans, hindering their reach and growth.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, non_sponsored_content_providers, payer,
    moderate, biographical, constrained, global).

% New companies and innovators struggle to compete with established, zero-rated services. The cost of entry is effectively raised, as they must either secure zero-rating deals (often prohibitively expensive) or face an uneven playing field.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, startups_and_innovators, payer,
    powerless, immediate, trapped, global).

% These groups actively campaign against zero-rating, arguing it violates the principles of an open internet and harms competition and consumer choice. They engage in public education, lobbying, and legal challenges.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, net_neutrality_advocates, observer,
    organized, generational, analytical, global).

% Government bodies responsible for telecommunications policy. They have the power to permit, regulate, or prohibit zero-rating practices, often balancing industry interests with consumer protection and competition concerns.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, telecom_regulators, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows Internet Service Providers and specific content providers to coordinate on offering certain content without it counting against user data caps, potentially increasing access to that content for users and creating new revenue streams for ISPs.
% TRANSFER_FUNCTION: Transfers market advantage and potential revenue from non-sponsored content providers and users (who face data caps or limited choice) to ISPs and sponsored content providers, in exchange for preferential data treatment.
% ABSENT_VOICES: Smaller content providers who cannot afford zero-rating deals, and users who prioritize an open, non-discriminatory internet over specific 'free' content. These voices are often drowned out by the marketing power of ISPs and large content companies.
% DISAPPEARANCE_RATIONALE: If zero-rating and its enforcement vanished overnight, ISPs would lose a significant revenue stream and a tool for customer acquisition/retention. Content consumption patterns would shift as all content became equally 'costly' in terms of data, leading to a more level playing field for content providers and potentially lower overall data costs for users. The mobile internet economy would reorganize around a more open access model.
% FOUNDING_PROBLEM: ISPs sought new revenue streams and ways to differentiate their services in a competitive market, while large content providers aimed to ensure their content reached users without data-cap friction.
% FOUNDING_PROBLEM_CORROBORATION: ISPs and sponsored content providers continue to assert the need for such arrangements to foster innovation and consumer choice. However, net neutrality advocates and competition authorities, citing economic analysis and user surveys, argue that the original problem is largely a pretext for rent-seeking and market control, with the arrangement now primarily serving to entrench incumbents.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__zero_rating_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__zero_rating_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__zero_rating_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(tcp_ip_interpretation__zero_rating_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__zero_rating_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.7) reflects the significant market advantage and revenue streams generated for ISPs and sponsored content providers, at the expense of a less open and competitive internet. Suppression (0.75) is high because ISPs actively enforce data caps and traffic management to maintain the zero-rating distinction, effectively suppressing alternatives for users and non-sponsored content providers. The moderate theater ratio (0.4) acknowledges that while some claims of 'innovation' and 'consumer choice' are made, a substantial portion of the activity is dedicated to maintaining market control and revenue generation. The increasing trend in all metrics over the interval reflects the growing entrenchment and extractive nature of zero-rating policies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of ISPs and sponsored content providers, zero-rating is an innovative business model that offers consumer choice and drives content consumption. From the perspective of net neutrality advocates, non-sponsored content providers, and many users, it is an anti-competitive practice that undermines the open internet. The engine's classification will highlight this divergence by computing different effective extraction values for each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Internet Service Providers and sponsored content providers are clear beneficiaries, gaining revenue and market share. Internet users, non-sponsored content providers, and startups are targets, bearing the costs of limited choice, competitive disadvantage, and higher barriers to entry. Telecom regulators and net neutrality advocates act as observers or potential agenda-setters, influencing the policy landscape.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_interpretation_ambiguity,
    'Is the TCP/IP protocol suite inherently neutral, requiring non-discrimination, or does it permit selective exemptions and differentiated treatment of traffic?',
    'Consensus among network architects and engineers on the fundamental design principles of TCP/IP, or a definitive legal/regulatory ruling that establishes a binding interpretation.',
    'If TCP/IP is deemed inherently neutral, the zero-rating reading would be reclassified as a Snare, as its coordination story would be entirely cover for extraction. If it is deemed to permit such differentiation, the Tangled Rope classification would be reinforced, acknowledging a genuine (though asymmetric) coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_interpretation_ambiguity, conceptual, 'Ambiguity regarding the fundamental design principles of TCP/IP and its implications for network neutrality.').

omega_variable(
    economic_impact_on_competition,
    'Does zero-rating genuinely foster innovation and consumer choice, or does it primarily entrench incumbent content providers and create insurmountable barriers for new entrants?',
    'Longitudinal economic studies comparing market dynamics in jurisdictions with and without zero-rating, focusing on startup success rates, content diversity, and consumer switching behavior.',
    'If studies show significant harm to competition and innovation, the extractiveness and suppression metrics would be revised upward, potentially pushing the classification closer to a Snare. If genuine innovation is demonstrated, the coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_on_competition, empirical, 'The true economic impact of zero-rating on market competition and innovation.').

omega_variable(
    consumer_welfare_tradeoff,
    'Do the benefits of ''free'' access to specific content outweigh the costs to users in terms of reduced choice, potential data cap overages for other content, and a less open internet?',
    'Comprehensive user surveys and behavioral economics studies that quantify the perceived value of zero-rated content versus the opportunity cost of restricted access to non-zero-rated content, and the impact on overall internet usage patterns.',
    'If the net consumer welfare is negative, the ''beneficiary'' role for internet_users would be re-evaluated, potentially shifting their directionality further towards a target, increasing the overall effective extraction of the constraint. If positive, the coordination aspect would be emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_welfare_tradeoff, preference, 'The net impact of zero-rating on consumer welfare, balancing perceived benefits against hidden costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__zero_rating_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(tcp__tr_t4, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(tcp__tr_t8, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(tcp__tr_t12, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(tcp__tr_t16, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(tcp__tr_t20, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(tcp__be_t4, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(tcp__be_t8, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement(tcp__be_t12, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(tcp__be_t16, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 16, 0.69).
narrative_ontology:measurement(tcp__be_t20, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 20, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(tcp__su_t4, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 4, 0.65).
narrative_ontology:measurement(tcp__su_t8, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(tcp__su_t12, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 12, 0.73).
narrative_ontology:measurement(tcp__su_t16, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 16, 0.74).
narrative_ontology:measurement(tcp__su_t20, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__zero_rating_reading, resource_allocation).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, net_neutrality_regulation).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, digital_divide_dynamics).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, content_monopolization_dynamics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
