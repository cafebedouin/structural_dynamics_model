% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__zero_rating_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Zero-Rating Reading of TCP/IP: Sponsored-Content Data Cap Exemptions
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   This story instantiates the zero-rating reading of the TCP/IP
 *   interpretation kernel: the protocol's silence on application-layer
 *   billing is read as authorization for ISPs to exempt sponsored content
 *   from data caps. This is structurally distinct from the neutrality reading
 *   (which reads the same protocol as mandating non-discrimination among
 *   packets) and the prioritization reading (which reads it as permitting
 *   differentiated service quality for network management purposes). All
 *   three are readings of the same underlying kernel — the protocol's silence
 *   on how carriers may treat traffic from different sources — but they
 *   authorize categorically different carrier behaviors and produce different
 *   beneficiary/victim structures. Zero-rating's distinctive delta is
 *   economic rather than technical: it does not touch packet routing or QoS,
 *   it touches billing, and its principal effect is to convert carrier
 *   billing discretion into a competitive moat for whichever content
 *   providers can afford exemption deals.
 *
 * KEY AGENTS:
 *   - isp_zero_rating_partners: agenda_setter/beneficiary (institutional/arbitrage) — designs and administers exemption programs, collects fees
 *   - incumbent_content_platforms: beneficiary (powerful/mobile) — captures default status among cap-constrained users
 *   - small_content_startups: payer (powerless/trapped) — priced out of the exemption market, structurally disadvantaged
 *   - capped_data_consumers: payer/beneficiary (powerless/constrained) — short-term relief, long-term steering
 *   - unaffiliated_isps: payer (moderate/constrained) — competitively disadvantaged versus scaled incumbents
 *   - telecom_regulators: observer (institutional/analytical) — adjudicates permissibility jurisdiction by jurisdiction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, 0.68).
domain_priors:suppression_score(tcp_ip_interpretation__zero_rating_reading, 0.58).
domain_priors:theater_ratio(tcp_ip_interpretation__zero_rating_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__zero_rating_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__zero_rating_reading, "Zero-Rating Reading of TCP/IP: Sponsored-Content Data Cap Exemptions").
narrative_ontology:topic_domain(tcp_ip_interpretation__zero_rating_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__zero_rating_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__zero_rating_reading, '909c88fa-79c2-4bcf-a151-bad365956e4d').
narrative_ontology:cs_kernel_codification('909c88fa-79c2-4bcf-a151-bad365956e4d', distributed).
narrative_ontology:cs_authority_grounding('909c88fa-79c2-4bcf-a151-bad365956e4d', distributed).
narrative_ontology:cs_reading_relation('909c88fa-79c2-4bcf-a151-bad365956e4d', tcp_ip_interpretation__neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('909c88fa-79c2-4bcf-a151-bad365956e4d', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_axiom('909c88fa-79c2-4bcf-a151-bad365956e4d', foundational, carrier_billing_discretion_is_permissible_absent_explicit_prohibition).
narrative_ontology:cs_axiom_status(carrier_billing_discretion_is_permissible_absent_explicit_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('909c88fa-79c2-4bcf-a151-bad365956e4d', carrier_billing_discretion_is_permissible_absent_explicit_prohibition, conventional).
narrative_ontology:cs_axiom('909c88fa-79c2-4bcf-a151-bad365956e4d', secondary, sponsored_exemption_serves_affordability_not_merely_incumbency).
narrative_ontology:cs_axiom_status(sponsored_exemption_serves_affordability_not_merely_incumbency, holdable).
narrative_ontology:cs_axiom_grounding('909c88fa-79c2-4bcf-a151-bad365956e4d', sponsored_exemption_serves_affordability_not_merely_incumbency, empirically_contingent).
narrative_ontology:cs_reference_frame('909c88fa-79c2-4bcf-a151-bad365956e4d', protocol_silence_as_permissive_default).
narrative_ontology:cs_drift_state('909c88fa-79c2-4bcf-a151-bad365956e4d', post_2016_regulatory_challenges, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('909c88fa-79c2-4bcf-a151-bad365956e4d', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, incumbent_content_platforms).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, isp_zero_rating_partners).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, small_content_startups).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, capped_data_consumers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, unaffiliated_isps).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, capped_data_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer data plans that exempt specific partnered applications or platforms from counting against subscriber data caps. Negotiates the exemption agreements, sets the technical criteria for qualifying traffic, and collects fees or reciprocal carriage benefits from participating content providers. Frames the practice as consumer-friendly pricing innovation and legitimate network management under the TCP/IP framework's silence on application-layer billing.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, isp_zero_rating_partners, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, isp_zero_rating_partners, beneficiary).

% Pay or negotiate to have their traffic zero-rated, making their service effectively free to data-capped users while rivals' traffic consumes the user's limited allowance. Already has scale and capital to strike these deals across many carriers, cementing default status among cap-constrained users regardless of underlying service quality.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, incumbent_content_platforms, beneficiary,
    powerful, generational, mobile, global).

% Cannot afford zero-rating deals or lack the negotiating leverage to strike them, so their traffic counts fully against users' data caps. Users facing tight data budgets systematically choose the zero-rated incumbent over the metered alternative, regardless of comparative quality — market entry is effectively gated by capital for carrier deals rather than product merit.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, small_content_startups, payer,
    powerless, biographical, trapped, national).

% Get short-term relief from data costs when using zero-rated apps, which reads as a benefit at the point of use. Over time their consumption patterns are steered toward whichever platforms bought exemptions, narrowing their functional choice set and making the exempted apps a default rather than a preference. Switching plans or ISPs to escape the steering is possible but costly and rarely done.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, capped_data_consumers, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, capped_data_consumers, beneficiary).

% Smaller or newer ISPs without the scale to negotiate exemption deals with major content platforms find themselves competitively disadvantaged versus larger carriers who can offer 'free' access to popular apps. Their only options are absorbing the competitive loss, seeking their own smaller-scale exemption deals, or lobbying for regulatory restriction of the practice.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, unaffiliated_isps, payer,
    moderate, biographical, constrained, national).

% Adjudicate whether zero-rating violates neutrality principles or is a permissible pricing innovation. Commission economic studies, take comments from all sides, and can approve, restrict, or ban the practice depending on which reading of the underlying protocol's neutrality commitments prevails in their jurisdiction.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, telecom_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Zero-rating solves a genuine problem for data-constrained users in markets with expensive or scarce data: it lets a carrier offer meaningfully cheaper access to some services when full unmetered access is not commercially viable, particularly in emerging markets with high per-megabyte costs.
% TRANSFER_FUNCTION: Moves competitive advantage and user attention from platforms that cannot afford exemption deals to those that can, mediated through the ISP's technical capacity to selectively meter traffic by source; the ISP itself collects fees or reciprocal carriage terms from the exemption sponsors.
% ABSENT_VOICES: Small content startups and open-internet advocates from the countries where zero-rating has been most aggressively deployed rarely have a formal seat at the carrier-platform negotiating table; regulators hear from ISPs and incumbent sponsors far more often than from the excluded startups who cannot afford lobbying presence.
% DISAPPEARANCE_RATIONALE: If selective exemption authority vanished, ISPs would need to meter all traffic identically, sponsored-content deals would dissolve, incumbent platforms would lose their cap-exemption advantage, and users would face uniform data costs across all services — competitive dynamics among content providers would shift toward quality and price rather than carrier-deal capital.
% FOUNDING_PROBLEM: In markets with expensive or capacity-constrained mobile data, full unmetered internet access was commercially unworkable for many carriers; zero-rating emerged as a way to give users some free access to at least a subset of services rather than none.
% FOUNDING_PROBLEM_CORROBORATION: ISPs and sponsored platforms attest the founding problem (data affordability) remains live and that zero-rating is the solution. Independent researchers, competition regulators in the EU and India, and digital-rights organizations outside the benefiting parties attest that in mature broadband markets the affordability problem is largely solved and the practice now functions primarily as a competitive-entrenchment mechanism for incumbents rather than an affordability bridge.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__zero_rating_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__zero_rating_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__zero_rating_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tcp_ip_interpretation__zero_rating_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__zero_rating_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored as substantial (0.68 at interval end) because the arrangement's steady-state function is to convert carrier-side billing discretion into durable competitive advantage for whichever platforms have the capital to negotiate exemptions — a transfer from unaffiliated competitors and startups to incumbents, mediated through consumer data-cap anxiety. Suppression is moderate (0.58): there is no direct coercion of startups, but the practical foreclosure of a competitive path (offering a comparable but non-exempted alternative) functions as suppression once caps are tight enough to steer behavior. Theater ratio (0.42) reflects a genuinely mixed practice — some markets and users derive real affordability benefit — but a growing share of the practice's visible framing (as pure consumer benefit) diverges from its function of entrenching incumbents.
 *
 * PERSPECTIVAL GAP:
 *   From the ISP/incumbent-platform seat, this is coordination: carriers solving an affordability problem by partnering with the services users most want. From the small-startup and unaffiliated-ISP seat, the identical billing mechanism operates as an entry barrier enforced by capital requirements rather than product merit. The engine should compute divergent per-seat types from these two positions given the same structural facts — that divergence is the data point, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   ISPs sit as agenda-setters who benefit from exemption fees and vertical partnership leverage. Incumbent platforms are pure beneficiaries — d near the beneficiary end — because they receive the exemption and could survive without it, gaining a durable moat they did not need to earn through service quality. Small content startups are targets — trapped, powerless — because the mechanism actively narrows the market the moment users are cap-constrained. Capped data consumers occupy an unusual mixed position: they receive genuine short-term relief (some d toward beneficiary) while being steered and having their long-run choice set narrowed (some d toward target) — this is exactly the dual-role case the schema's secondary_role field exists for.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (data affordability in constrained markets) is not fully dead — it remains live in several emerging markets — but the corroboration split (ISPs/platforms say live; regulators and independent researchers in mature markets say largely resolved and now serving entrenchment) is exactly the founding_problem_status=contested case R5 exists to surface. Treating zero-rating uniformly as either 'pure Rope born of affordability necessity' or 'pure Snare of incumbent capture' would mislabel the practice in whichever markets it does not fit; the tangled_rope classification with victims AND beneficiaries preserves both readings simultaneously rather than forcing a single verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    billing_silence_vs_neutrality_mandate,
    'Does TCP/IP''s technical silence on application-layer billing constitute affirmative authorization for selective data-cap exemptions, or does the end-to-end design principle implicitly forbid source-based differential treatment even at the billing layer?',
    'This is the core committer disagreement between this reading and the neutrality_reading. Resolution would require either an authoritative technical-body ruling on what the protocol''s silence implies, or a legal/regulatory determination (as in India''s 2016 zero-rating ban or the EU''s net neutrality guidelines) establishing which reading governs a given jurisdiction.',
    'If the neutrality reading is adopted as authoritative, zero-rating arrangements of the kind modeled here become impermissible and this constraint''s beneficiary structure collapses; if the zero-rating reading holds, the practice persists and likely intensifies as incumbents deepen exemption deals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(billing_silence_vs_neutrality_mandate, conceptual, 'Committer disagreement: whether protocol silence on billing implies permission or is foreclosed by the neutrality principle.').

omega_variable(
    affordability_vs_entrenchment_balance,
    'In any given market, what fraction of zero-rating''s effect is genuine affordability bridging for data-constrained users versus incumbent competitive entrenchment?',
    'Comparative market studies tracking user platform-choice diversity and startup market entry rates in jurisdictions with versus without zero-rating, controlling for underlying data cost and market maturity.',
    'In markets where affordability dominates, the coordination function is real and the tangled_rope classification''s beneficiary-side legitimacy is well-grounded; in mature markets where entrenchment dominates, the same practice may be better classified as approaching snare given how thin the residual coordination function has become.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(affordability_vs_entrenchment_balance, empirical, 'Whether the affordability justification is genuinely load-bearing or has become a legitimating cover story.').

omega_variable(
    consumer_short_vs_long_run_benefit,
    'Do capped data consumers'' short-term savings from zero-rated access outweigh their long-term loss of platform diversity and negotiating leverage?',
    'Longitudinal study of user welfare and platform-switching behavior in zero-rating markets versus neutral-pricing control markets.',
    'Determines whether consumers should be modeled predominantly as beneficiaries (short-run framing) or predominantly as targets (long-run framing) — currently authored as dual-role to preserve both readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consumer_short_vs_long_run_benefit, empirical, 'Whether consumer benefit from zero-rating is net positive over a realistic time horizon.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__zero_rating_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tcp__tr_t4, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(tcp__tr_t8, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(tcp__tr_t12, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(tcp__tr_t16, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(tcp__tr_t20, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(tcp__tr_t24, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tcp__be_t4, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(tcp__be_t8, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(tcp__be_t12, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(tcp__be_t16, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(tcp__be_t20, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(tcp__be_t24, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(tcp__su_t4, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(tcp__su_t8, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(tcp__su_t12, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(tcp__su_t16, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(tcp__su_t20, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(tcp__su_t24, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__zero_rating_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__zero_rating_reading, 0.12).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__prioritization_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the tcp_ip_interpretation kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle: neutrality_reading (non-discrimination mandate, likely rope/mountain-adjacent at the protocol level), prioritization_reading (permitted QoS differentiation for network management, likely tangled_rope depending on whether prioritization is sold competitively), and this zero_rating_reading (billing-layer sponsored exemptions, tangled_rope). The readings are linked bidirectionally via affects_constraints because regulatory or technical resolution of one reading structurally constrains what the others can claim — an authoritative neutrality ruling forecloses zero-rating's legitimating premise, while zero-rating's proliferation creates political pressure that shapes how regulators eventually rule on the neutrality question.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
