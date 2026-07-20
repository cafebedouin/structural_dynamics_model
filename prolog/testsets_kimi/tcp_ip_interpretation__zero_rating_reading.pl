% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__zero_rating_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Zero-Rating Reading of TCP/IP Interpretation
 *   domain: technology governance / internet policy / telecommunications law
 *
 * SUMMARY:
 *   This constraint instantiates the zero_rating_reading of the
 *   tcp_ip_interpretation kernel: the claim that TCP/IP, as a transport
 *   infrastructure, permits Internet service providers to enter selective
 *   commercial arrangements exempting partnered content from subscriber data
 *   caps. The reading is contested against a neutrality_reading
 *   (non-discrimination as protocol logic) and a prioritization_reading (QoS
 *   differentiation). Under this reading, incumbent platforms gain structural
 *   advantage through ISP partnerships, while unaffiliated competitors face
 *   elevated entry barriers because their traffic counts against caps. The
 *   constraint is authored as a Tangled Rope: it carries a genuine
 *   coordination function (lower subscriber costs for selected services,
 *   ISP-platform revenue alignment) alongside asymmetric extraction from
 *   non-partnered providers.
 *
 * KEY AGENTS:
 *   - isp_operators: Agenda-setter (institutional/arbitrage/national) â designs metering and partnership terms
 *   - incumbent_platforms: Primary beneficiary (powerful/arbitrage/global) â captures market share via exemptions
 *   - competing_content_providers: Primary target (moderate/constrained/national) â bears competitive disadvantage
 *   - unaffiliated_startups: Secondary target (powerless/trapped/national) â faces entry barriers
 *   - mobile_subscribers: Dual-positioned beneficiary/payer (organized/constrained/national) â receives data subsidy but loses choice
 *   - telecom_regulators: Observer (institutional/analytical/national) â adjudicates legality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, 0.68).
domain_priors:suppression_score(tcp_ip_interpretation__zero_rating_reading, 0.6).
domain_priors:theater_ratio(tcp_ip_interpretation__zero_rating_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__zero_rating_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__zero_rating_reading, "Zero-Rating Reading of TCP/IP Interpretation").
narrative_ontology:topic_domain(tcp_ip_interpretation__zero_rating_reading, "technology governance / internet policy / telecommunications law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__zero_rating_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__zero_rating_reading, '44cef338-4b49-4dc0-9e53-223d1e9728ed').
narrative_ontology:cs_kernel_codification('44cef338-4b49-4dc0-9e53-223d1e9728ed', distributed).
narrative_ontology:cs_authority_grounding('44cef338-4b49-4dc0-9e53-223d1e9728ed', distributed).
narrative_ontology:cs_reading_relation('44cef338-4b49-4dc0-9e53-223d1e9728ed', tcp_ip_interpretation__neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('44cef338-4b49-4dc0-9e53-223d1e9728ed', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_axiom('44cef338-4b49-4dc0-9e53-223d1e9728ed', foundational, selective_exemption_commercially_legitimate).
narrative_ontology:cs_axiom_status(selective_exemption_commercially_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('44cef338-4b49-4dc0-9e53-223d1e9728ed', selective_exemption_commercially_legitimate, conventional).
narrative_ontology:cs_axiom('44cef338-4b49-4dc0-9e53-223d1e9728ed', foundational, sponsored_data_advances_consumer_access).
narrative_ontology:cs_axiom_status(sponsored_data_advances_consumer_access, holdable).
narrative_ontology:cs_axiom_grounding('44cef338-4b49-4dc0-9e53-223d1e9728ed', sponsored_data_advances_consumer_access, instrumental).
narrative_ontology:cs_reference_frame('44cef338-4b49-4dc0-9e53-223d1e9728ed', permissive_transport_layer).
narrative_ontology:cs_drift_state('44cef338-4b49-4dc0-9e53-223d1e9728ed', contemporary_telecom_policy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('44cef338-4b49-4dc0-9e53-223d1e9728ed', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, isp_operators).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, incumbent_platforms).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, mobile_subscribers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, competing_content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, unaffiliated_startups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, mobile_subscribers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design mobile data plans with byte caps and negotiate commercial partnerships to exempt selected platforms from those caps. Technically enforce metering at the network edge. Justify the practice as a consumer benefit and a cost-recovery mechanism. Collect subscriber fees and, in some arrangements, direct platform payments.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, isp_operators, agenda_setter,
    institutional, generational, arbitrage, national).

% Negotiate zero-rating agreements with ISPs so their traffic does not count against subscriber data limits. Gain market share in price-sensitive demographics where data budgets constrain usage. Their competitive position is structurally insulated from unaffiliated rivals.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, incumbent_platforms, beneficiary,
    powerful, generational, arbitrage, global).

% Offer services that consume subscriber data caps while exempt incumbents do not. Face higher effective user-acquisition costs and lower engagement from budget-constrained consumers. Cannot match the sponsorship deals due to smaller balance sheets and lower ISP leverage.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, competing_content_providers, payer,
    moderate, biographical, constrained, national).

% New entrants without ISP partnerships face a steep adoption barrier: every trial costs subscribers scarce data. The exemption regime raises the floor for viable market entry, effectively trapping innovators outside the zero-rated walled garden.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, unaffiliated_startups, payer,
    powerless, biographical, trapped, national).

% Receive direct data-budget relief for partnered platforms, lowering their monthly mobile costs. Over time, their choice set narrows as non-exempt services become prohibitively expensive to explore. They are the conduit through which the constraint reshapes platform competition.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, mobile_subscribers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, mobile_subscribers, payer).

% Adjudicate whether zero-rating violates net-neutrality frameworks or consumer-protection law. They can permit, condition, or prohibit the practice. Their rulings determine the legal envelope within which the constraint operates.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, telecom_regulators, observer,
    institutional, generational, analytical, national).

narrative_ontology:fixing_cost_class(tcp_ip_interpretation__zero_rating_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns ISP and platform incentives around subscriber acquisition in price-sensitive mobile markets, lowering perceived data costs for selected services through sponsorship arrangements.
% TRANSFER_FUNCTION: Moves data-cap budget relief from ISPs and partnered platforms to subscribers for selected services, while moving competitive disadvantage from exempt incumbents to unaffiliated content providers who must compete for attention under a data tax.
% ABSENT_VOICES: Unaffiliated startups and non-partnered content providers are absent from the ISP-platform negotiating table. Net-neutrality advocates participate in regulatory proceedings but are structurally outgunned by the ISP-incumbent coalition.
% DISAPPEARANCE_RATIONALE: If selective data exemptions disappeared overnight, subscriber data consumption would rebalance across services, unaffiliated platforms would face lower entry barriers, and the commercial ISP-platform partnership layer would reorganize around non-discriminatory pricing.
% FOUNDING_PROBLEM: How to recover mobile infrastructure costs and maintain subscriber growth in markets where consumers are highly price-sensitive and data caps limit usage.
% FOUNDING_PROBLEM_CORROBORATION: ISPs and incumbent platforms attest to the cost-recovery and access problem. Independent economists and net-neutrality advocates contest the framing, noting that flat-rate and non-discriminatory models are viable; corroboration from outside the benefiting parties is split and politically contested.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__zero_rating_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__zero_rating_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__zero_rating_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tcp_ip_interpretation__zero_rating_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__zero_rating_reading, 0.68, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.68) reflects the substantial but not total extraction from competitors: zero-rating does not block unaffiliated services entirely but raises their effective price to consumers. Suppression (0.60) captures the structural suppression of alternatives via data-cap stickiness and the regulatory-contractual enclosure of the subscriber relationship. Theater ratio (0.42) reflects the growing performative framing of zero-rating as 'free data for consumers' masking the competitive distortion underneath. Accessibility collapse (0.65) is high because once subscriber habits and platform-ISP contracts solidify, non-exempt services become practically inaccessible to price-sensitive users. Resistance (0.55) reflects ongoing but partially successful net-neutrality advocacy and regulatory pushback in some jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   The ISP seat experiences the arrangement as a legitimate commercial coordination that funds infrastructure and attracts subscribers. The incumbent platform seat experiences it as a competitive moat. The unaffiliated provider seat experiences the same technical phenomenon (traffic metering) as a discriminatory tax on market entry. The engine computes this divergence from structural data: identical protocol behavior has opposite valence depending on partnership status.
 *
 * DIRECTIONALITY LOGIC:
 *   ISP operators and incumbent platforms are declared beneficiaries (low d, extraction damped or inverted into subsidy). Unaffiliated competitors and startups are declared victims (high d, extraction amplified). Mobile subscribers sit near symmetric: they receive a direct subsidy (free data) but their choice set is constrained; their d is moderated by their constrained exit and organized power. Directionality is structurally derived from these declarations plus exit options; no override is needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring both beneficiaries and victims for Tangled Rope. A purely coordinative reading (Rope) would fail the victim gate; a purely extractive reading (Snare) would fail to acknowledge the genuine subscriber benefit and ISP-platform coordination. The founding problem â cost recovery and access in mobile broadband â is contested but not dead, preventing automatic Piton classification. The theater ratio is below 0.5, so Piton is not supported.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    zero_rating_coordination_boundary,
    'Is zero-rating a genuine welfare-enhancing coordination mechanism, or does the consumer benefit mask a structurally extractive barrier to competition?',
    'Comparative market analysis across jurisdictions that banned versus permitted zero-rating, measuring entrant success rates and consumer surplus over multi-year horizons.',
    'If consumer surplus is positive and entry is not suppressed, the constraint trends toward Rope; if entry is suppressed despite short-term consumer gains, Tangled Rope or Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_rating_coordination_boundary, empirical, 'Whether the consumer benefit is real enough to offset competitive extraction').

omega_variable(
    kernel_reading_contingency,
    'Does TCP/IP''s technical architecture normatively mandate non-discrimination, or is it a permissive substrate whose governance is entirely delegated to commercial and regulatory layers?',
    'Historical and sociotechnical analysis of IETF norms, RFC 1958 end-to-end arguments, and the evolution from academic to commercial internet governance.',
    'If the neutrality_reading is the structurally correct kernel reading, then this constraint is a false framing of the protocol; if the kernel is genuinely under-determined, then the zero_rating_reading is a live conventional interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Whether the zero-rating reading is a live interpretation or a misattribution of commercial practice to protocol logic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__zero_rating_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp_zr_tr_t0, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tcp_zr_tr_t3, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 3, 0.3).
narrative_ontology:measurement(tcp_zr_tr_t6, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(tcp_zr_tr_t9, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 9, 0.4).
narrative_ontology:measurement(tcp_zr_tr_t12, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 12, 0.42).

% Extraction over time
narrative_ontology:measurement(tcp_zr_be_t0, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tcp_zr_be_t3, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(tcp_zr_be_t6, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(tcp_zr_be_t9, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 9, 0.65).
narrative_ontology:measurement(tcp_zr_be_t12, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 12, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tcp_zr_su_t0, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(tcp_zr_su_t3, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 3, 0.55).
narrative_ontology:measurement(tcp_zr_su_t6, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(tcp_zr_su_t9, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 9, 0.68).
narrative_ontology:measurement(tcp_zr_su_t12, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 12, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__zero_rating_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
