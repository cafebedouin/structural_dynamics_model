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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: TCP/IP Zero-Rating Exemption Authorization
 *   domain: technology/telecommunications/internet_governance
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested TCP/IP
 *   interpretation kernel: the zero-rating reading asserts that TCP/IP and
 *   its governance allow ISPs to create selective data-cap exemptions for
 *   sponsored content. This reading authorizes ISPs to partner with large
 *   content platforms, exempting those platforms' traffic from user data caps
 *   while non-exempt traffic counts normally. The reading's beneficiaries are
 *   large incumbents that can negotiate favorable terms and ISP operators
 *   collecting side value; its victims are competitive entrants unable to
 *   negotiate exemptions and low-bandwidth users whose effective cap shrinks.
 *   The constraint is claimed as tangled_rope because it combines genuine ISP
 *   congestion-management coordination with asymmetric extraction favoring
 *   incumbents. Sibling readings (neutrality_reading, prioritization_reading)
 *   decompose the same kernel differently, claiming different foundational
 *   premises about what TCP/IP permits.
 *
 * KEY AGENTS:
 *   - incumbent_platform_operators: Large platforms (Netflix, Meta, YouTube) that negotiate zero-rating agreements; gain user reach advantage over smaller competitors
 *   - isp_operators: Broadband operators that set exemption terms, negotiate with platforms, and enforce via traffic shaping; control the decision-making
 *   - competitive_content_providers: Startups and smaller platforms without negotiation leverage; pay the full data cost while incumbents appear free
 *   - low_bandwidth_users: Users with limited data plans who benefit from zero-rated platforms but are harmed by the effective cap reduction and incumbent subsidy
 *   - network_neutrality_advocates: Civil society excluded from the arrangement but influential in regulatory pressure
 *   - telecommunications_regulators: Observers with enforcement power but divided interpretation of TCP/IP principles
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, 0.68).
domain_priors:suppression_score(tcp_ip_interpretation__zero_rating_reading, 0.61).
domain_priors:theater_ratio(tcp_ip_interpretation__zero_rating_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__zero_rating_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__zero_rating_reading, "TCP/IP Zero-Rating Exemption Authorization").
narrative_ontology:topic_domain(tcp_ip_interpretation__zero_rating_reading, "technology/telecommunications/internet_governance").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__zero_rating_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__zero_rating_reading, '8cc2b6ea-8766-43a3-aa81-c970e54c1b08').
narrative_ontology:cs_kernel_codification('8cc2b6ea-8766-43a3-aa81-c970e54c1b08', fixed_text).
narrative_ontology:cs_authority_grounding('8cc2b6ea-8766-43a3-aa81-c970e54c1b08', lineage).
narrative_ontology:cs_interpretation_layer_present('8cc2b6ea-8766-43a3-aa81-c970e54c1b08').
narrative_ontology:cs_reading_relation('8cc2b6ea-8766-43a3-aa81-c970e54c1b08', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('8cc2b6ea-8766-43a3-aa81-c970e54c1b08', tcp_ip_interpretation__prioritization_reading, influences).
narrative_ontology:cs_axiom('8cc2b6ea-8766-43a3-aa81-c970e54c1b08', foundational, isp_partnership_exemptions_permissible).
narrative_ontology:cs_axiom_status(isp_partnership_exemptions_permissible, holdable).
narrative_ontology:cs_axiom_grounding('8cc2b6ea-8766-43a3-aa81-c970e54c1b08', isp_partnership_exemptions_permissible, conventional).
narrative_ontology:cs_axiom('8cc2b6ea-8766-43a3-aa81-c970e54c1b08', foundational, content_discrimination_via_exemption_lawful).
narrative_ontology:cs_axiom_status(content_discrimination_via_exemption_lawful, holdable).
narrative_ontology:cs_axiom_grounding('8cc2b6ea-8766-43a3-aa81-c970e54c1b08', content_discrimination_via_exemption_lawful, empirically_contingent).
narrative_ontology:cs_reference_frame('8cc2b6ea-8766-43a3-aa81-c970e54c1b08', isp_network_management_authority).
narrative_ontology:cs_drift_state('8cc2b6ea-8766-43a3-aa81-c970e54c1b08', contemporary_platform_partnership_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8cc2b6ea-8766-43a3-aa81-c970e54c1b08', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, incumbent_platform_operators).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, isp_operators).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, competitive_content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, low_bandwidth_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, low_bandwidth_users).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__zero_rating_reading, network_operator_business_model_autonomy).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__zero_rating_reading, sponsored_content_as_valid_service_differentiation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large platforms (video streaming, social media, search) that can negotiate zero-rating agreements with ISPs to exempt their traffic from data caps. They gain user reach by making their services effectively cheaper than competitors, while those competitors pay the full data cost. Their market position compounds: they can afford the negotiation overhead and have the traffic volume to interest ISPs; smaller entrants cannot.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, incumbent_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Internet service providers that set the terms of zero-rating arrangements, negotiate with content platforms, and enforce the exemptions through traffic shaping. They justify this as network management and revenue diversification; they set the policy, negotiate the terms, collect side payments or receive traffic commitments, and maintain the technical infrastructure to distinguish sponsored from non-sponsored traffic.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, isp_operators, agenda_setter,
    institutional, generational, mobile, national).

% Startups and smaller platforms that cannot negotiate favorable zero-rating terms. They pay the full data cost when users access their services, putting them at a structural disadvantage: users see their service as consuming bandwidth while incumbent platforms appear 'free' (exempt from caps). Market entry is effectively taxed; growth is constrained by the gap between their cost per user acquired and incumbents' lower apparent cost.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, competitive_content_providers, payer,
    moderate, biographical, constrained, global).

% Users with limited data plans who benefit from zero-rated platforms (they use them 'for free' within their cap) but are harmed by the constraint's structure: their cap is effectively smaller for everything else, and the cap itself creates artificial scarcity that zero-rating arrangements exploit. They cannot negotiate better terms and cannot easily switch ISPs. The arrangement subsidizes their use of incumbent platforms while making alternatives appear more expensive.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, low_bandwidth_users, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, low_bandwidth_users, beneficiary).

% Civil society and public-interest organizations arguing that zero-rating violates end-to-end principles and favors incumbents. They are excluded from the negotiation process: the decisions happen between ISPs and large platforms, and the advocates' objections are not part of the arrangement's governance, though they influence regulatory pressure and public discourse.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, network_neutrality_advocates, excluded,
    organized, biographical, constrained, global).

% National communications regulators (FCC, BEREC, similar bodies) that oversee whether zero-rating arrangements violate net neutrality rules. Different regulators have taken different positions: some permit zero-rating as lawful network management, others restrict it as discriminatory. They hold enforcement power but lack consensus on the interpretation itself.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, telecommunications_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables ISPs to offer data-conscious users cheaper access to popular platforms through negotiated exemptions, reducing the apparent cost of high-bandwidth services for price-sensitive segments. Solves the ISP's problem of managing congestion while maintaining revenue in a competitive broadband market.
% TRANSFER_FUNCTION: Moves market access and user acquisition advantage from competitive entrants to incumbent platforms by exempting incumbent traffic from user data caps while charging competitors' traffic against the cap. Also moves revenue from users and non-sponsored content providers to ISP and incumbent platform operators.
% ABSENT_VOICES: Competitive startups and public-interest organizations advocating for open access. They would object to the exclusion of their content and the regulatory framework permitting ISPs to make these distinctions, but they are not seated at the negotiation table. Their absence is structural: smaller platforms have no leverage to demand exemption, and their objections do not constrain the arrangement.
% DISAPPEARANCE_RATIONALE: If zero-rating exemptions vanished, all traffic would count equally against user data caps, destroying the negotiated advantages and removing the incentive structure that favors incumbent platforms. ISP revenue would depend on data-cap levels and total usage rather than on side deals with content providers. Competitive platforms would compete on service quality rather than on exemption status; market entry barriers would lower. Users would face uniformly-priced bandwidth with no sponsor subsidies.
% FOUNDING_PROBLEM: ISPs operate in a competitive broadband market with congestion management challenges and saturating demand. Zero-rating arrangements provide a revenue source beyond connectivity fees and allow targeting of services to price-sensitive users without lowering baseline plan prices.
% FOUNDING_PROBLEM_CORROBORATION: ISP industry attests the problem is live and zero-rating is a necessary tool for managing congestion and competing in broadband markets. Independent telecommunications economists attest the problem statement is partly true (revenue pressure is real) but mostly misrepresents the constraint's function: the founding problem (congestion + revenue) could be solved by transparent congestion pricing, and zero-rating was chosen because it favors existing relationships and incumbent platforms. Regulatory filings and antitrust investigations show ISPs explicitly designed zero-rating to increase engagement with their partnership platforms, not primarily for congestion management.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__zero_rating_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__zero_rating_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__zero_rating_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
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
 *   Extractiveness is 0.68 (interval end) because the arrangement systematically transfers market advantage to incumbents via an apparent subsidy (exemption from data caps) that is actually a tax on competitors. The extraction compounds: incumbents benefit from lower user acquisition cost, which they reinvest in network effects, which deepens the exemption's value, which locks in ISP partnerships. Suppression is 0.61 because the constraint's persistence requires active enforcement (traffic classification, cap enforcement for non-exempt traffic, negotiation of exclusivity) and requires keeping competitive alternatives from gaining exemption parity. Theater is moderate (0.42): the congestion-management justification is partly real (ISPs do face congestion challenges) but increasingly performs a secondary role as the arrangement matures — most growth in zero-rating deals after year 6 is partnership-driven, not congestion-driven, suggesting the performance of congestion management is becoming larger than the actual function. The measurement series tracks this transition: extractiveness and theater both rise as the arrangement stabilizes, while suppression plateaus (the enforcement load is set early and holds steady). All metrics share one time grid: every metric is authored at t=0, 3, 6, 12, 18, 24.
 *
 * PERSPECTIVAL GAP:
 *   From the ISP and incumbent platform seat, the arrangement is legitimate network management with value-creation through partnership: they enabled a new service tier without raising baseline prices. From the competitive-entrant and low-bandwidth-user seat, the same structure is enforced market segmentation that reduces their options. The engine should compute Rope or Light-Snare from the ISP/incumbent seat (coordination function, beneficiary value) and Snare or Heavy-Tangled-Rope from the competitive-entrant seat (no coordination value to them, pure extraction). This divergence IS the measurement the corpus takes; it is not an error in the JSON claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent platforms are beneficiaries (d near 0.0) — they collect exemption advantage and can afford the negotiation overhead. ISP operators are the agenda-setters (power=institutional, exit_options=mobile, so derived d is moderate-to-high in the setter position) — they set terms and enforce. Competitive entrants are targets (d near 1.0): powerless relative to the negotiators, constrained exit (must eventually accept the exemption gap or exit the market), no leverage in the arrangement. Low-bandwidth users are split: they benefit from exemptions for platforms they use heavily (partial beneficiary direction) but are harmed by the effective cap reduction and the incumbent subsidy (partial target direction) — derived d is symmetric but the composition is mixed. No overrides needed; the derivation captures the seat structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's founding problem (ISP congestion + revenue diversification in competitive markets) is live but increasingly de-centered from the actual function. ISP filings show zero-rating deals are now driven by incumbent platform partnerships, not congestion management. The arrangement persists because it generates rents for both ISP and incumbent-platform operators, and because the regulatory interpretation remains contested — different jurisdictions permit or restrict zero-rating at different thresholds. If the founding problem (congestion management) were solved by other means (explicit congestion pricing, unlimited data plans), the arrangement would persist anyway because it serves the hidden function (incumbent advantage). This is not yet mandatrophy (a function-dead constraint maintained by pure inertia) but it is a candidate: the announced founding problem and the observed function are diverging. Declaring the founding_problem_status as 'live' but the disappearance_verdict as 'world_rearranges' reflects this: the world would rearrange because beneficiaries depend on it, but the dependence is not bottlenecked to the original problem statement anymore.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    congestion_vs_rent_seeking,
    'Does zero-rating persist primarily as a solution to ISP congestion management, or primarily as a rent-extraction mechanism favoring incumbent platforms?',
    'Temporal analysis of zero-rating deal timing relative to network congestion metrics: if deals cluster during high-congestion periods, the congestion rationale is live; if deals cluster with incumbent-platform partnerships regardless of congestion status, rent-seeking is the driver. Also: ISP network investment levels — if zero-rating replaced infrastructure investment, it is rent-seeking; if it supplements investment, it is congestion management.',
    'If primarily congestion management, the constraint is a legitimate coordination response to a technical problem and supports the ISP/incumbent beneficiary framing. If primarily rent-seeking, the congestion justification is a cover story and the constraint is pure extraction riding on a coordination narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congestion_vs_rent_seeking, empirical, 'Whether zero-rating solves a technical bottleneck or extracts rents by disguising extraction as technical management.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Do the neutrality reading and the zero-rating reading (this reading) logically foreclose each other within a single regulatory framework, or do they coexist as different positions held by different jurisdictions and regulators?',
    'Doctrinal analysis of network neutrality jurisprudence and statutory language: if one reading''s core premise directly contradicts the other''s (e.g., one reading claims exemptions are categorically forbidden, the other claims they are categorically permitted), they foreclose. If both are live positions defended within the regulatory apparatus (as they are: EU regulations restrict zero-rating; US regulations permit it), they coexist despite contradiction.',
    'If they foreclose, the framework''s authority structure is splitting and one reading will eventually be judicially/legislatively overridden. If they coexist, the kernel remains ambiguous and both readings persist as jurisdictional variants. This determines whether the zero-rating reading is a permanent fixture (coexistence) or a temporary incumbent-favorable state pending neutrality resolution (foreclosure by the neutrality reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the zero-rating and neutrality readings are logically incompatible or merely held by different regulatory authorities.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.61) primarily structural (ISPs actively enforce exemptions via traffic shaping and rate-limiting) or internalized (competitive platforms have internalized the assumption that exemptions are normal and necessary, and suppress their own value proposition accordingly)?',
    'Post-exemption-removal observation: if competitors rapidly rebrand their services as higher-value-for-full-price when exemptions disappear (as in jurisdictions that banned zero-rating), suppression was primarily structural. If competitors continue to discount their price or quality expectations even after exemptions are removed, suppression is partially internalized.',
    'Structural suppression can be lifted by regulatory intervention; internalized suppression persists and requires market re-education and competitive action. Higher internalization means the constraint''s effective suppression is higher than the measured value suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is imposed externally (ISP infrastructure and enforcement) or carried by competitors themselves (learned inferiority).').

omega_variable(
    kernel_reading_ambiguity,
    'Is the TCP/IP interpretation kernel genuinely ambiguous (susceptible to multiple defensible readings), or do the technical and normative facts resolve toward one reading?',
    'Canonical TCP/IP standards documentation (RFCs 791, 793, etc.) and historical design intent vs. regulatory jurisprudence: if the standards are silent or ambiguous on content-discrimination, the kernel is ambiguous and all three readings coexist legitimately. If the standards contain clear guidance, the kernel is under-determined only by interpretation (regulatory choice given technical facts), not by the facts themselves.',
    'If the kernel is genuinely ambiguous, all three readings remain live and the constraint''s type depends on which reading governs in a given jurisdiction. If the kernel is technically resolved but interpreted differently by regulators, the readings are jurisdictional policy variants, not multiple valid interpretations of the same ambiguous kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the TCP/IP interpretation kernel is fundamentally ambiguous or is clear but interpreted differently by different regulatory regimes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__zero_rating_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(tcp__tr_t0, observed).
narrative_ontology:measurement(tcp__tr_t3, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 3, 0.31).
narrative_ontology:measurement_basis(tcp__tr_t3, observed).
narrative_ontology:measurement(tcp__tr_t6, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement_basis(tcp__tr_t6, observed).
narrative_ontology:measurement(tcp__tr_t12, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(tcp__tr_t12, observed).
narrative_ontology:measurement(tcp__tr_t18, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 18, 0.41).
narrative_ontology:measurement_basis(tcp__tr_t18, observed).
narrative_ontology:measurement(tcp__tr_t24, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement_basis(tcp__tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(tcp__be_t0, observed).
narrative_ontology:measurement(tcp__be_t3, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 3, 0.54).
narrative_ontology:measurement_basis(tcp__be_t3, observed).
narrative_ontology:measurement(tcp__be_t6, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 6, 0.59).
narrative_ontology:measurement_basis(tcp__be_t6, observed).
narrative_ontology:measurement(tcp__be_t12, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement_basis(tcp__be_t12, observed).
narrative_ontology:measurement(tcp__be_t18, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 18, 0.67).
narrative_ontology:measurement_basis(tcp__be_t18, observed).
narrative_ontology:measurement(tcp__be_t24, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(tcp__be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(tcp__su_t0, observed).
narrative_ontology:measurement(tcp__su_t3, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 3, 0.55).
narrative_ontology:measurement_basis(tcp__su_t3, observed).
narrative_ontology:measurement(tcp__su_t6, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement_basis(tcp__su_t6, observed).
narrative_ontology:measurement(tcp__su_t12, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement_basis(tcp__su_t12, observed).
narrative_ontology:measurement(tcp__su_t18, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 18, 0.61).
narrative_ontology:measurement_basis(tcp__su_t18, observed).
narrative_ontology:measurement(tcp__su_t24, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 24, 0.61).
narrative_ontology:measurement_basis(tcp__su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__zero_rating_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__zero_rating_reading, 0.18).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__prioritization_reading).

% DUAL FORMULATION NOTE:
% The tcp_ip_interpretation kernel decomposes into three structurally distinct constraints via different readings of what TCP/IP and its governance permit regarding content discrimination. Neutrality_reading (Mountain/stable) claims end-to-end principles forbid exemptions entirely. Prioritization_reading (Rope/Tangled-Rope) claims differentiated quality is permitted but not exemptions. Zero-rating_reading (THIS constraint, Tangled-Rope) claims exemptions are permitted when negotiated with content providers. Each reading has a distinct epsilon, beneficiary/victim structure, and regulatory implications. All three readings are linked via network.affects_constraints to show family membership and decomposition of the contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
