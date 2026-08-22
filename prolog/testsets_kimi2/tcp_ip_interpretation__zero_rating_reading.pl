% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__zero_rating_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Zero-Rating Data Exemption Arrangements
 *   domain: technology governance / internet policy / telecommunications law
 *
 * SUMMARY:
 *   This constraint story instantiates the zero_rating_reading of the
 *   tcp_ip_interpretation kernel: the claim that TCP/IP's layered, permissive
 *   architecture authorizes ISPs to enter commercial partnerships that exempt
 *   selected content from user data caps. The constraint is not the protocol
 *   itself but the institutional practice of zero-rating that rests on an
 *   interpretation of the protocol stack as commercially neutral at the
 *   transport layer. It coordinates genuine value (consumers access
 *   data-heavy services without cap anxiety) while extracting from
 *   competitors and consumers through competitive foreclosure. The claim is
 *   Tangled Rope: the coordination function (enabling data-intensive app
 *   usage under caps) is real, but the same structure asymmetrically
 *   advantages incumbents who can pay for exemption and raises barriers to
 *   entry for competitors who cannot.
 *
 * KEY AGENTS:
 *   - major_isps (agenda_setter, institutional/mobile) â set partnership terms and enforce technical exemptions
 *   - incumbent_platforms (beneficiary, institutional/mobile) â gain user lock-in via cap exemptions
 *   - competing_content_providers (payer, moderate/constrained) â lose traffic and viability without ISP deals
 *   - consumer_subscribers (payer/beneficiary, organized/constrained) â get free data but face reduced choice
 *   - regulatory_authorities (observer, institutional/analytical) â adjudicate neutrality compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, 0.62).
domain_priors:suppression_score(tcp_ip_interpretation__zero_rating_reading, 0.58).
domain_priors:theater_ratio(tcp_ip_interpretation__zero_rating_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__zero_rating_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__zero_rating_reading, "Zero-Rating Data Exemption Arrangements").
narrative_ontology:topic_domain(tcp_ip_interpretation__zero_rating_reading, "technology governance / internet policy / telecommunications law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__zero_rating_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__zero_rating_reading, '5a8db10d-861d-4d02-80b6-4952e6aeb60c').
narrative_ontology:cs_kernel_codification('5a8db10d-861d-4d02-80b6-4952e6aeb60c', formalized).
narrative_ontology:cs_authority_grounding('5a8db10d-861d-4d02-80b6-4952e6aeb60c', practice).
narrative_ontology:cs_reading_relation('5a8db10d-861d-4d02-80b6-4952e6aeb60c', tcp_ip_interpretation__neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('5a8db10d-861d-4d02-80b6-4952e6aeb60c', tcp_ip_interpretation__prioritization_reading, influences).
narrative_ontology:cs_axiom('5a8db10d-861d-4d02-80b6-4952e6aeb60c', foundational, commercial_exemption_norm).
narrative_ontology:cs_axiom_status(commercial_exemption_norm, holdable).
narrative_ontology:cs_axiom_grounding('5a8db10d-861d-4d02-80b6-4952e6aeb60c', commercial_exemption_norm, conventional).
narrative_ontology:cs_reference_frame('5a8db10d-861d-4d02-80b6-4952e6aeb60c', permissive_transport_infrastructure).
narrative_ontology:cs_drift_state('5a8db10d-861d-4d02-80b6-4952e6aeb60c', post_neutrality_rollback_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5a8db10d-861d-4d02-80b6-4952e6aeb60c', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, major_isps).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, incumbent_platforms).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, competing_content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, consumer_subscribers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, consumer_subscribers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate last-mile and mobile data networks; negotiate zero-rating partnerships with large content platforms, implement technical and billing systems to exempt partnered traffic from user data caps, and collect sponsorship fees or equivalent consideration.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, major_isps, agenda_setter,
    institutional, generational, mobile, national).

% Large content platforms that contract with ISPs for zero-rated status; their services do not count against consumer data caps, increasing user engagement and locking out smaller competitors who cannot match the arrangement.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, incumbent_platforms, beneficiary,
    institutional, biographical, mobile, global).

% Smaller or unaffiliated content providers whose traffic counts against data caps; they lose prospective users to zero-rated incumbents and lack the capital or market power to negotiate equivalent ISP partnerships.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, competing_content_providers, payer,
    moderate, biographical, constrained, global).

% Mobile internet users subject to data caps; they receive short-term savings when accessing zero-rated services, but their effective choice set is narrowed to partnered platforms and long-term prices reflect reduced competitive pressure.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, consumer_subscribers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, consumer_subscribers, beneficiary).

% Telecommunications and competition regulators that adjudicate whether zero-rating violates net neutrality or competition law; their enforcement posture determines whether the constraint persists or is prohibited.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% Civil society and technical advocates arguing for strict non-discrimination at the network layer; they are structurally excluded from bilateral ISP-platform commercial negotiations and depend on regulatory or judicial processes to contest the arrangement.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, net_neutrality_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates ISP-content provider partnerships to deliver bandwidth-intensive services to consumers who face data caps, aligning network usage with commercial sponsorship and reducing consumer price sensitivity for partnered services.
% TRANSFER_FUNCTION: Moves data consumption toward zero-rated incumbent platforms and away from competing services; moves subscription and advertising revenue from unaffiliated providers to partnered platforms and sponsorship fees to ISPs; transfers competitive viability from market entrants to established zero-rated incumbents.
% ABSENT_VOICES: Competing content providers who cannot afford sponsorship deals, net neutrality advocates arguing for strict non-discrimination, and consumer representatives concerned about long-term foreclosure are excluded from ISP-platform bilateral negotiations.
% DISAPPEARANCE_RATIONALE: If zero-rating vanished, ISP-platform sponsorship contracts would terminate, consumer data consumption would redistribute across non-exempt services, competitive entry barriers in mobile content markets would fall, and mobile data pricing models would shift away from cap-exemption partnerships.
% FOUNDING_PROBLEM: Mobile data caps created user anxiety that suppressed adoption of bandwidth-heavy streaming and application services; ISPs and platforms sought a mechanism to stimulate usage without eliminating the cap-based pricing model.
% FOUNDING_PROBLEM_CORROBORATION: ISPs and incumbent platforms attest the problem is still live, citing spectrum scarcity. Net neutrality advocates, independent economists, and regulators in pro-neutrality jurisdictions attest that the founding problem has shifted from genuine scarcity management to marketplace foreclosure; European regulatory findings and Indian TRAI rulings from outside the benefiting parties support the foreclosure reading.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__zero_rating_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__zero_rating_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__zero_rating_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tcp_ip_interpretation__zero_rating_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__zero_rating_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) reflects the substantial competitive foreclosure effect: zero-rating functions as a paid prioritization scheme implemented via billing rather than QoS, transferring market share from unaffiliated providers to partnered incumbents. Suppression (0.58) is moderate-high because the arrangement requires active technical enforcement (deep packet inspection or billing-system integration to exempt partnered traffic) and contractual exclusion of rivals. Theater ratio (0.45) captures the performative framing of 'free data for consumers' that masks the foreclosure mechanism. Accessibility collapse (0.48) indicates that while alternatives (unlimited data plans, strict neutrality regulation) exist, they are partly suppressed by ISP lobbying and regulatory capture. Resistance (0.55) reflects ongoing net neutrality advocacy, regulatory proceedings, and jurisdictional bans (e.g., India, EU). The measurement series show extraction and theater rising as the practice matures and regulatory pushback intensifies.
 *
 * PERSPECTIVAL GAP:
 *   The ISP seat experiences the constraint as a legitimate commercial coordination mechanism that improves consumer welfare and network economics. The competing content provider seat experiences it as an enforced extraction of their competitive opportunity. The consumer seat is split: short-term subsidy (free data) versus long-term cost (foreclosed choice). The engine computes these divergences from the structural data rather than adjudicating them.
 *
 * DIRECTIONALITY LOGIC:
 *   major_isps and incumbent_platforms are structural beneficiaries: ISPs collect sponsorship fees and retain cap-based pricing power; platforms gain attention share and lock-in (d near 0.0â0.2). competing_content_providers are structural targets: they bear the cost of reduced traffic and higher effective barriers to user acquisition (d near 0.9â1.0). consumer_subscribers sit near symmetric but with net target directionality because the short-term subsidy is visible while the long-term foreclosure cost is diffuse and deferred (d ~0.55â0.6). regulatory_authorities have analytical exit and bear no extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâdata cap anxiety suppressing adoptionâwas arguably live in early mobile broadband. However, the constraint persists and has intensified even as network capacity expanded and unlimited plans became technically feasible, indicating mandatrophy risk. Because the constraint lacks a sunset clause, it cannot be a Scaffold. Classifying it as Tangled Rope correctly captures that the coordination function (enabling usage under caps) is genuine but the persistent, asymmetric extraction (foreclosure) now dominates the mature form. A Snare classification would miss the real coordination benefit to consumers and ISPs; a Rope classification would miss the competitive victimization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the zero-rating reading an inherent feature of TCP/IP''s layered architecture, or an extraneous commercial imposition that could be removed without altering the protocol stack?',
    'Comparative analysis of protocol specifications (RFCs) versus operational ISP practices; if zero-rating requires no protocol-level changes and is implemented entirely at the billing and partnership layer, it is extraneous to the kernel.',
    'If extraneous, the constraint is a policy-level Tangled Rope or Snare, not an inherent property of internet architecture; if inherent, it suggests a deeper structural feature of packet-switched networks under commercial control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether zero-rating is intrinsic to TCP/IP or an extraneous commercial layer').

omega_variable(
    data_cap_artificiality,
    'Are mobile data caps primarily a response to genuine spectrum scarcity, or an artificial scarcity mechanism that creates the conditions for zero-rating extraction?',
    'Engineering studies of congestion economics and cost-structure analysis of mobile data delivery; jurisdictions with unlimited data plans provide a natural experiment.',
    'If caps are artificial, the coordination rationale for zero-rating collapses and the extraction fraction rises; if genuine, some extraction is the price of scarcity management.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_cap_artificiality, empirical, 'Whether data caps reflect real scarcity or artificial scarcity').

omega_variable(
    consumer_welfare_net_sign,
    'Does the consumer surplus from zero-rated data exceed the deadweight loss from competitive foreclosure?',
    'Longitudinal market concentration metrics and consumer price indices in jurisdictions with and without zero-rating prohibitions.',
    'If consumer surplus exceeds foreclosure loss, the directionality for consumer_subscribers shifts toward symmetric; if not, they are net targets.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consumer_welfare_net_sign, empirical, 'Net welfare impact on consumers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__zero_rating_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp_ip_zr_tr_t0, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tcp_ip_zr_tr_t5, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement(tcp_ip_zr_tr_t10, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(tcp_ip_zr_tr_t15, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(tcp_ip_zr_tr_t20, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(tcp_ip_zr_tr_t25, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 25, 0.45).

% Extraction over time
narrative_ontology:measurement(tcp_ip_zr_be_t0, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(tcp_ip_zr_be_t5, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(tcp_ip_zr_be_t10, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(tcp_ip_zr_be_t15, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(tcp_ip_zr_be_t20, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(tcp_ip_zr_be_t25, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(tcp_ip_zr_su_t0, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(tcp_ip_zr_su_t5, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(tcp_ip_zr_su_t10, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(tcp_ip_zr_su_t15, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 15, 0.53).
narrative_ontology:measurement(tcp_ip_zr_su_t20, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(tcp_ip_zr_su_t25, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__zero_rating_reading, resource_allocation).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__prioritization_reading).

% DUAL FORMULATION NOTE:
% This constraint is the zero_rating_reading of the tcp_ip_interpretation kernel, decomposed per the Îµ-invariance principle from neutrality_reading and prioritization_reading because the natural-language label 'TCP/IP interpretation' conflates structurally distinct claims about protocol nondiscrimination, traffic management quality-of-service, and commercial data-cap exemptions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
