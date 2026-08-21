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
 *   This constraint story instantiates the 'zero_rating_reading' of the
 *   broader 'tcp_ip_interpretation' kernel. It describes how the TCP/IP
 *   protocols are interpreted to allow Internet Service Providers (ISPs) to
 *   offer selective exemptions for sponsored content from user data caps.
 *   This practice, often framed as a consumer benefit, structurally
 *   advantages incumbent content platforms and raises competitive barriers
 *   for new entrants. The claimed type of 'tangled_rope' reflects the dual
 *   function of perceived user benefit alongside significant extraction and
 *   suppression.
 *
 * KEY AGENTS:
 *   - Internet Service Providers (ISPs): Primary agenda-setter (institutional/arbitrage) – implement and enforce zero-rating.
 *   - Sponsored Content Providers: Primary beneficiary (powerful/mobile) – gain preferential access to users.
 *   - Internet Users on Data Caps: Primary payer (powerless/constrained) – receive 'free' content but pay with limited choice.
 *   - Non-Sponsored Content Providers: Payer (moderate/constrained) – disadvantaged by lack of zero-rating.
 *   - Startups and Innovators: Payer (powerless/trapped) – face high barriers to entry.
 *   - Net Neutrality Advocates: Observer (organized/analytical) – actively resist zero-rating.
 *   - Telecom Regulators: Agenda-setter (institutional/analytical) – can permit or prohibit zero-rating.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, 0.7).
domain_priors:suppression_score(tcp_ip_interpretation__zero_rating_reading, 0.8).
domain_priors:theater_ratio(tcp_ip_interpretation__zero_rating_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__zero_rating_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__zero_rating_reading, "TCP/IP Interpretation: Zero-Rating Reading").
narrative_ontology:topic_domain(tcp_ip_interpretation__zero_rating_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__zero_rating_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__zero_rating_reading, '6b3607fb-0f32-48ab-ac75-2319150cec86').
narrative_ontology:cs_kernel_codification('6b3607fb-0f32-48ab-ac75-2319150cec86', fixed_text).
narrative_ontology:cs_authority_grounding('6b3607fb-0f32-48ab-ac75-2319150cec86', practice).
narrative_ontology:cs_interpretation_layer_present('6b3607fb-0f32-48ab-ac75-2319150cec86').
narrative_ontology:cs_reading_relation('6b3607fb-0f32-48ab-ac75-2319150cec86', tcp_ip_interpretation__neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('6b3607fb-0f32-48ab-ac75-2319150cec86', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_axiom('6b3607fb-0f32-48ab-ac75-2319150cec86', foundational, isp_commercial_freedom).
narrative_ontology:cs_axiom_status(isp_commercial_freedom, holdable).
narrative_ontology:cs_axiom_grounding('6b3607fb-0f32-48ab-ac75-2319150cec86', isp_commercial_freedom, conventional).
narrative_ontology:cs_axiom('6b3607fb-0f32-48ab-ac75-2319150cec86', secondary, user_choice_maximization_via_differentiation).
narrative_ontology:cs_axiom_status(user_choice_maximization_via_differentiation, holdable).
narrative_ontology:cs_axiom_grounding('6b3607fb-0f32-48ab-ac75-2319150cec86', user_choice_maximization_via_differentiation, instrumental).
narrative_ontology:cs_reference_frame('6b3607fb-0f32-48ab-ac75-2319150cec86', commercial_flexibility_framework).
narrative_ontology:cs_drift_state('6b3607fb-0f32-48ab-ac75-2319150cec86', contemporary_regulatory_contestation, gap(stable, minor, false)).
narrative_ontology:cs_created_at('6b3607fb-0f32-48ab-ac75-2319150cec86', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, internet_service_providers).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, sponsored_content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, internet_users_on_data_caps).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, non_sponsored_content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, startups_and_innovators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% ISPs implement zero-rating policies, partnering with content providers to exempt their traffic from user data caps. They benefit from new revenue streams, customer lock-in, and reduced churn. They actively enforce these policies through network management and billing systems.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, internet_service_providers, agenda_setter,
    institutional, generational, arbitrage, global).

% These are large content companies (e.g., streaming services, social media platforms) that pay ISPs to have their content zero-rated. They gain preferential access to users, increased engagement, and a competitive advantage over non-sponsored content.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, sponsored_content_providers, beneficiary,
    powerful, biographical, mobile, global).

% Users benefit from accessing specific content without it counting against their data caps, which can be perceived as 'free data'. However, they pay with limited choice, potential higher overall data costs (as data caps may be set lower to encourage zero-rating), and a less open internet experience. Their exit options are limited by available ISP choices.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, internet_users_on_data_caps, payer,
    powerless, immediate, constrained, local).

% These are content providers who cannot or choose not to pay for zero-rating. Their content is disadvantaged, as users on data caps may be less likely to access it. They face higher barriers to competition and innovation.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, non_sponsored_content_providers, payer,
    moderate, biographical, constrained, global).

% New companies and innovators face significant barriers to entry if they cannot afford to compete with established, zero-rated services. Their ability to reach users and grow is severely hampered, leading to market consolidation.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, startups_and_innovators, payer,
    powerless, biographical, trapped, global).

% These groups actively campaign against zero-rating, arguing it violates the principles of net neutrality and creates a two-tiered internet. They engage in public education, lobbying, and legal challenges.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, net_neutrality_advocates, observer,
    organized, generational, analytical, global).

% Government bodies responsible for telecommunications policy. They interpret existing laws and regulations regarding internet traffic management and can permit, restrict, or prohibit zero-rating practices based on their assessment of market competition and consumer welfare.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, telecom_regulators, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows Internet Service Providers (ISPs) and specific content providers to coordinate on offering certain content without it counting against user data caps, potentially increasing consumption of that content and providing a perceived benefit to users.
% TRANSFER_FUNCTION: Transfers market advantage and potential revenue from non-sponsored content providers and internet users (via data cap limitations) to ISPs and sponsored content providers.
% ABSENT_VOICES: Smaller content providers who cannot afford to participate in zero-rating schemes, and users who prioritize an open, non-discriminatory internet over specific 'free' content. Their concerns are often voiced by net neutrality advocates but are not directly represented in the commercial agreements.
% DISAPPEARANCE_RATIONALE: If zero-rating and its enforcement vanished overnight, ISPs would lose a significant revenue stream, content consumption patterns would shift away from preferentially treated services, and competition for content delivery would become more level. The mobile internet economy would reorganize around a more uniform pricing model for data.
% FOUNDING_PROBLEM: ISPs sought new revenue streams and ways to differentiate their services in competitive markets, while content providers aimed to ensure guaranteed reach and engagement with their user base, especially in regions with prevalent data caps.
% FOUNDING_PROBLEM_CORROBORATION: ISPs and sponsored content providers continue to assert the problem is live, citing ongoing needs for revenue, market differentiation, and user engagement. Net neutrality advocates and some economists, however, argue that the founding problem is largely addressed by market competition and the arrangement now primarily serves as a mechanism for rent extraction and market control; this is supported by independent economic analyses and regulatory findings in various jurisdictions.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__zero_rating_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__zero_rating_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__zero_rating_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The high extractiveness (0.7) reflects the significant market advantage and revenue streams generated for ISPs and sponsored content providers, often at the expense of a truly open internet. Suppression (0.8) is high due to the active enforcement by ISPs to block alternative payment routing and the structural disadvantage imposed on non-sponsored content. The theater ratio (0.4) indicates that while there's a genuine narrative of 'free data' for users, a substantial portion of the constraint's operation is dedicated to maintaining market control and revenue generation. Accessibility collapse (0.7) is high because while alternatives exist, they are made significantly less attractive or costly to access. Resistance (0.7) is also high, reflecting ongoing legal and advocacy battles against zero-rating practices.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of ISPs and sponsored content providers, zero-rating is an innovative service offering that benefits users and fosters competition. From the perspective of non-sponsored content providers, startups, and net neutrality advocates, it is an anti-competitive practice that undermines the open internet. The engine's classification will highlight this divergence by computing different effective extraction values for each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   ISPs and sponsored content providers are clear beneficiaries, as they directly gain revenue and market share. Internet users on data caps are payers, as their choices are constrained and they indirectly bear costs through a less open internet, even if they perceive a direct benefit. Non-sponsored content providers and startups are also payers, facing significant competitive disadvantages. Telecom regulators and net neutrality advocates act as observers or potential agenda-setters, influencing the constraint's future.
 *
 * MANDATROPHY ANALYSIS:
 *   The initial justification for zero-rating often centers on consumer benefit and market innovation. However, as extractiveness and theater ratio increase, the constraint risks becoming a snare where the coordination story (user benefit) becomes a cover for rent extraction. The 'live' but 'contested' status of the founding problem, coupled with rising extractiveness, suggests a potential drift towards mandatrophy, where the original mandate is overshadowed by extractive functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    zero_rating_competitive_impact,
    'Does zero-rating genuinely foster competition and innovation, or does it entrench incumbents and create barriers to entry for new services?',
    'Longitudinal economic studies comparing market concentration, startup success rates, and content diversity in jurisdictions with and without zero-rating policies.',
    'If found to entrench incumbents, the constraint''s effective extractiveness for non-sponsored content providers and startups would be higher, pushing its classification closer to a Snare. If it genuinely fosters competition, the coordination function would be emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_rating_competitive_impact, empirical, 'Assessing the true competitive effects of zero-rating practices.').

omega_variable(
    tcp_ip_inherent_neutrality,
    'Is the TCP/IP protocol inherently neutral, requiring non-discriminatory treatment of all data, or does it permit and even encourage differentiated service offerings like zero-rating?',
    'Deep historical and technical analysis of the original design principles of TCP/IP, combined with expert consensus from network architects and computer scientists, to determine the protocol''s foundational intent regarding traffic discrimination.',
    'If TCP/IP is found to be inherently neutral, this ''zero_rating_reading'' would be reclassified as a Snare, as its premise would contradict the foundational ''mountain'' of the internet''s architecture. If it permits differentiation, the ''tangled_rope'' classification would be more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tcp_ip_inherent_neutrality, conceptual, 'Conceptual ambiguity regarding the foundational neutrality of TCP/IP protocols.').

omega_variable(
    user_perception_of_free_data,
    'Do internet users fully understand the trade-offs involved in zero-rating (e.g., limited choice, potential for higher overall data costs) or is the ''free data'' framing a form of cognitive capture?',
    'Behavioral economics studies and user surveys designed to measure informed consent and understanding of zero-rating''s implications, rather than just stated preference for ''free'' content.',
    'If cognitive capture is significant, the effective suppression and extractiveness for internet users would be higher, as their ''choice'' is less informed, strengthening the ''snare'' aspects of the constraint. If users are fully informed, the coordination aspect is more legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_perception_of_free_data, empirical, 'Assessing the degree of informed consent among users regarding zero-rating.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__zero_rating_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(tcp__tr_t6, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(tcp__tr_t12, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(tcp__tr_t18, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(tcp__tr_t24, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(tcp__tr_t30, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(tcp__be_t6, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(tcp__be_t12, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(tcp__be_t18, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(tcp__be_t24, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 24, 0.69).
narrative_ontology:measurement(tcp__be_t30, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 30, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(tcp__su_t6, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(tcp__su_t12, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 12, 0.75).
narrative_ontology:measurement(tcp__su_t18, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 18, 0.78).
narrative_ontology:measurement(tcp__su_t24, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 24, 0.79).
narrative_ontology:measurement(tcp__su_t30, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 30, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__zero_rating_reading, resource_allocation).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, net_neutrality_regulation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'tcp_ip_interpretation' kernel, alongside 'neutrality_reading' and 'prioritization_reading'. Each reading represents a distinct structural claim about what TCP/IP 'allows' or 'requires', leading to different ε values and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
