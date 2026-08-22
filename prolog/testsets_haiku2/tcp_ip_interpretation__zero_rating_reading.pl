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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: tcp_ip_interpretation__zero_rating_reading
 *   human_readable: TCP/IP Zero-Rating Exemption Authorization (Reading)
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   This constraint instantiates the zero-rating reading of the contested
 *   TCP/IP kernel. Under this reading, TCP/IP's technical design permits
 *   network operators to selectively exempt content providers from data caps
 *   through commercial partnerships. The reading authorizes ISPs to
 *   distinguish content based on partnership status rather than technical
 *   characteristics or user preference. This reading is ONE of three
 *   structurally distinct interpretations of how TCP/IP and net neutrality
 *   rules should apply to data exemptions. The other readings
 *   (neutrality_reading: TCP/IP requires content-neutral treatment;
 *   prioritization_reading: TCP/IP permits differentiated service quality for
 *   legitimate network management) offer competing frameworks for the same
 *   technical capability. The zero-rating reading claims that exemptions for
 *   sponsored content are a legitimate use of network operator authority. The
 *   extracted metrics describe an asymmetric arrangement whose persistence
 *   depends on suppressing competitive alternatives—not because the reading's
 *   core claim is false, but because the reading's application benefits
 *   incumbent platforms at the expense of competitive entrants.
 *
 * KEY AGENTS:
 *   - major_isps: institutional gatekeepers controlling last-mile infrastructure; set partnership criteria; capture partnership revenue
 *   - incumbent_content_platforms: powerful, already-dominant actors; can afford to subsidize ISP deals; grow faster via zero-rating advantage
 *   - competitive_startups: powerless, trapped; cannot afford ISP partnerships; face structural data-cost disadvantage
 *   - low_income_users: powerless, trapped; effective choice set constrained by zero-rating incentives; data budget disappears faster for non-exempted content
 *   - innovation_advocates: moderate power, excluded from technical/commercial decisions; argue for end-to-end principle; voice appears in regulation but not in implementation
 *   - regulators: observers; interpret whether zero-rating is lawful network management or discrimination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, 0.68).
domain_priors:suppression_score(tcp_ip_interpretation__zero_rating_reading, 0.72).
domain_priors:theater_ratio(tcp_ip_interpretation__zero_rating_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__zero_rating_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__zero_rating_reading, "TCP/IP Zero-Rating Exemption Authorization (Reading)").
narrative_ontology:topic_domain(tcp_ip_interpretation__zero_rating_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__zero_rating_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__zero_rating_reading, '4c60ce1e-6a40-4ff6-a57f-83b788d71bab').
narrative_ontology:cs_kernel_codification('4c60ce1e-6a40-4ff6-a57f-83b788d71bab', distributed).
narrative_ontology:cs_authority_grounding('4c60ce1e-6a40-4ff6-a57f-83b788d71bab', extraction).
narrative_ontology:cs_interpretation_layer_present('4c60ce1e-6a40-4ff6-a57f-83b788d71bab').
narrative_ontology:cs_reading_relation('4c60ce1e-6a40-4ff6-a57f-83b788d71bab', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c60ce1e-6a40-4ff6-a57f-83b788d71bab', tcp_ip_interpretation__prioritization_reading, influences).
narrative_ontology:cs_axiom('4c60ce1e-6a40-4ff6-a57f-83b788d71bab', foundational, isp_partnership_authorization_legitimate).
narrative_ontology:cs_axiom_status(isp_partnership_authorization_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('4c60ce1e-6a40-4ff6-a57f-83b788d71bab', isp_partnership_authorization_legitimate, conventional).
narrative_ontology:cs_axiom('4c60ce1e-6a40-4ff6-a57f-83b788d71bab', secondary, content_discrimination_by_partnership_permissible).
narrative_ontology:cs_axiom_status(content_discrimination_by_partnership_permissible, holdable).
narrative_ontology:cs_axiom_grounding('4c60ce1e-6a40-4ff6-a57f-83b788d71bab', content_discrimination_by_partnership_permissible, instrumental).
narrative_ontology:cs_reference_frame('4c60ce1e-6a40-4ff6-a57f-83b788d71bab', network_operator_partnership_authorization).
narrative_ontology:cs_drift_state('4c60ce1e-6a40-4ff6-a57f-83b788d71bab', contemporary_net_neutrality_regulation_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('4c60ce1e-6a40-4ff6-a57f-83b788d71bab', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, incumbent_content_platforms).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, major_isps).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, independent_content_creators).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, competitive_startups).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, low_income_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, low_income_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control backbone infrastructure and last-mile delivery. Authorize selective zero-rating partnerships where favored content partners (Netflix, Facebook, Instagram) are exempted from users' data caps while competitors count against the cap. Set the exemption criteria and benefit directly from the partnerships through revenue sharing and strategic alignment. Position zero-rating as consumer-friendly data management.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, major_isps, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, major_isps, beneficiary).

% Already have scale and traffic dominance. Negotiate zero-rating deals with ISPs at favorable terms, passing the cost of cap exemption onto smaller competitors. Can afford to subsidize ISP revenue shares as part of user acquisition cost; smaller competitors cannot. Grow faster and face lower effective data costs than rivals, creating compounding advantage.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, incumbent_content_platforms, beneficiary,
    powerful, generational, arbitrage, global).

% Create content but lack bargaining power to negotiate zero-rating deals with ISPs. Their traffic counts against users' data caps while incumbents' traffic does not. This raises their effective user acquisition cost and visibility disadvantage. Have to compete on a tilted field where the platform's data advantage is built into the network pricing structure, not justified by content quality or user preference.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, independent_content_creators, payer,
    moderate, biographical, constrained, global).

% Cannot afford to subsidize ISP partnerships and lack the installed user base to justify them. Their content is priced structurally more expensively to users (counts against caps) while incumbents are priced less (exempted). Entry barrier is not product quality or innovation but data-cost disadvantage baked into the network layer. Exit means abandoning the platform or region entirely.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, competitive_startups, payer,
    powerless, biographical, trapped, global).

% Face tight data caps due to cost. Zero-rating of incumbent platforms makes those platforms appear free while alternatives consume real budget. Users with limited data effectively cannot explore competitive content because the cap structure incentivizes the zero-rated incumbents. Benefit from cheaper access to dominant platforms but lose choice because exit to alternatives is now prohibitively expensive.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, low_income_users, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, low_income_users, beneficiary).

% Argue that zero-rating violates the end-to-end principle and Internet openness by delegating content selection to ISPs. Their voice appears in regulatory and academic debate but is structurally excluded from ISP partnership decisions and the technical governance layer where zero-rating is implemented. Would advocate for content-neutral data treatment if they had a seat at enforcement.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, innovation_advocates, excluded,
    moderate, biographical, constrained, national).

% Monitor whether zero-rating constitutes unlawful discrimination under net neutrality rules or permitted network management. Their interpretation of TCP/IP's technical baseline determines whether the practice is legal or requires remedy. Currently divided: some treat zero-rating as permissible, others as veiled discrimination.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__zero_rating_reading, major_isps).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__zero_rating_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables ISPs to manage congestion by allowing selective data cap exemptions as an incentive mechanism for traffic reduction on constrained networks. Theoretically allows network operators to optimize resource allocation by pricing data consumption differentially based on actual network impact.
% TRANSFER_FUNCTION: Transfers competitive advantage from content creators without ISP partnership agreements to those with such agreements. Moves revenue from competitive startups (via reduced user reach and higher effective data cost) to incumbent platforms and ISPs (via partnership fees and increased user lock-in). Transfers data budget capacity from low-income users to incumbent platforms.
% ABSENT_VOICES: Competitive content creators, users in regions without zero-rating partnerships, and innovation advocates are structurally excluded from the partnership negotiation process. They would argue that zero-rating privileges incumbent platforms contrary to the end-to-end principle, but have no seat in the technical or commercial decision-making layer where exemptions are authorized.
% DISAPPEARANCE_RATIONALE: If zero-rating exemptions were prohibited overnight, data usage patterns would rebalance toward content-neutral treatment; competitive startups would face lower relative data costs; incumbent platforms would lose their exempted status and face user friction; ISPs would lose zero-rating revenue streams. The market structure would reorganize around actual network cost rather than partnership status.
% FOUNDING_PROBLEM: Mobile data plans initially had genuinely tight capacity constraints in early LTE deployments; operators needed mechanisms to manage peak load and incentivize users to moderate consumption. Zero-rating offered a way to make premium content 'free' to users while managing ISP costs.
% FOUNDING_PROBLEM_CORROBORATION: ISPs attest capacity management remains necessary and zero-rating is efficient. Telecom engineers confirm congestion exists but increasingly note that capacity has grown substantially and modern traffic shaping handles congestion without zero-rating. Competitive content providers and net neutrality advocates attest the founding capacity problem is largely solved and the arrangement persists primarily for revenue and control. Regulatory testimony and independent network analysis support the diminished-problem reading.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__zero_rating_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__zero_rating_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__zero_rating_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tcp_ip_interpretation__zero_rating_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__zero_rating_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.48 to 0.68 across the interval, tracking the proliferation of zero-rating partnerships and their cumulative effect on competitive disadvantage. Theater rises from 0.22 to 0.41: the network management justification (real capacity benefits) shrinks as a share of the constraint's operation while partnership-leverage grows. Suppression rises from 0.58 to 0.72: competitive content is actively excluded from exemption eligibility; the constraint's persistence requires ISPs to refuse partnerships with startups and maintain the exemption criteria that privilege incumbents. The metrics describe a Tangled Rope: genuine coordination function (congestion management, user data management) fused with substantial asymmetric extraction (competitive disadvantage, incumbent lock-in). The claim/metric divergence is the engine's job to detect—the reading itself claims Tangled Rope (coordination + enforcement of partnership criteria), but the metrics show extraction amplifying over time as congestion pressure eases and extraction motivation hardens.
 *
 * PERSPECTIVAL GAP:
 *   The ISP and incumbent platform seats should compute differently from the startup and low-income user seats. From the ISP's seat, zero-rating is efficient coordination—data management by price signals, partnerships that align incentives. From the startup's seat, the same structure is an entry barrier enforced by ISP preference for established platforms. From the low-income user's seat, zero-rating is both benefit (cheaper access to incumbents) and constraint (no choice). The engine computes these divergences from power and exit; the authored claim does not adjudicate which perspective is 'correct'—both are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   ISPs sit at d ≈ 0.2 (beneficiary: collect partnership revenue, control platform relationships, defend their gatekeeping role). Incumbent platforms sit at d ≈ 0.1 (beneficiary: lower effective data costs, user lock-in, competitive advantage). Competitive startups sit at d ≈ 0.9 (target: higher effective data costs, reduced visibility, structural disadvantage). Low-income users sit at d ≈ 0.75 (target: tight data caps, incentivized toward incumbents, reduced choice). The directionality spread is large because the zero-rating reading creates stark beneficiary/target asymmetry—it explicitly authorizes differentiation based on partnership status, which is precisely what creates the extraction gradient. No directionality override is needed; the structural data derive the right d values directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (congestion management on capacity-constrained networks) is contested: ISPs claim it is still live, network engineers note capacity has grown substantially, competitors claim it is functionally dead. The constraint persists because ISPs benefit from zero-rating revenue and partner relationships, not because the coordination function is essential. If the founding problem died (capacity is no longer scarce), the arrangement should sunset; instead, it persists via enforcement of partnership criteria that block competitive entry. This is the Tangled Rope signature: coordination function + asymmetric extraction + active enforcement. The theater ratio rising from 0.22 to 0.41 is the signal—as real congestion management becomes less necessary, the partnership-leverage component becomes more visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_constraint_status,
    'Are data capacity constraints on mobile networks still genuine bottlenecks requiring zero-rating-style traffic management, or has infrastructure growth and optimization rendered capacity management less pressing than it was at zero-rating''s inception?',
    'Network measurement studies comparing actual utilization, congestion events, and overprovisioning ratios across ISPs and regions; engineering assessment of whether zero-rating is the least-cost tool for actual congestion or a proxy for value extraction.',
    'If capacity constraints are substantially reduced, the coordination function claim weakens and the constraint reclassifies toward snare. If constraints remain material, the Tangled Rope classification holds. The extraction ratio (theater_ratio rising) suggests the founding problem importance has diminished relative to partnership leverage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_constraint_status, empirical, 'Whether the founding congestion-management problem remains live.').

omega_variable(
    partnership_negotiation_asymmetry,
    'Do ISPs systematically refuse zero-rating partnerships with competitive startups for legitimate network-management reasons, or as a gatekeeping mechanism to protect incumbent platforms?',
    'Regulatory discovery of ISP partnership criteria and decision records; comparison of startup versus incumbent acceptance rates; analysis of whether partnership refusals correlate with competitive threat to incumbent platforms or with actual network characteristics of the content.',
    'If refusals are cost-based and neutral across content types, the constraint is pure coordination with pricing differences that happen to favor incumbents. If refusals are deliberately exclusionary, the suppression metric is understated and the constraint is more snare-like than Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partnership_negotiation_asymmetry, empirical, 'Whether zero-rating partnership gatekeeping is legitimate network management or strategic foreclosure.').

omega_variable(
    reading_foreclosure_boundary,
    'Does the zero-rating reading structurally foreclose the neutrality reading within a single regulatory framework, or can both coexist as competing policy positions?',
    'Jurisdictional analysis: where zero-rating is legally authorized, is content-neutral treatment prohibited, or permitted as an alternative compliance path? In jurisdictions with net neutrality rules that forbid zero-rating, what does the law say about the technical TCP/IP capability versus the regulatory permission to use it?',
    'If zero-rating authorization forecloses neutrality (a regulator permits zero-rating and forbids neutral-only ISPs), the reading relation is ''forecloses''. If both can coexist as permitted architectures (an ISP can choose zero-rating or neutrality), the relation is ''coexists_with''. Current evidence suggests coexistence (US permits both as policy choices, EU heavily constrains zero-rating but does not forbid it absolutely), pointing to ''coexists_with''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Logical incompatibility of zero-rating and neutrality readings.').

omega_variable(
    suppression_mechanism_source,
    'Is the measured suppression (0.72) primarily structural (ISP infrastructure control makes competition technically difficult) or internalized (startups believe they cannot compete at scale without ISP partnerships and self-censor from entering)?',
    'Natural experiment from jurisdictions that ban zero-rating: if startup entry and success rates rise after zero-rating is prohibited, suppression was partially structural. Post-exit survey of startup founders about barriers to entry in zero-rating vs. neutral networks.',
    'If suppression is mostly structural, fixing requires regulatory intervention on partnership criteria. If suppression is internalized, some startups might succeed even without ISP partnership if given confidence that neutral routing is mandatory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_source, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__zero_rating_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(tcp__tr_t5, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement(tcp__tr_t10, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(tcp__tr_t15, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(tcp__tr_t20, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(tcp__tr_t25, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(tcp__be_t5, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(tcp__be_t10, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(tcp__be_t15, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(tcp__be_t20, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(tcp__be_t25, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(tcp__su_t5, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(tcp__su_t10, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(tcp__su_t15, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(tcp__su_t20, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(tcp__su_t25, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__zero_rating_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__zero_rating_reading, 0.12).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__prioritization_reading).

% DUAL FORMULATION NOTE:
% The tcp_ip_interpretation kernel admits three structurally distinct constraints, one per reading. This story (zero_rating_reading) claims TCP/IP permits selective exemptions for sponsored content. The neutrality_reading claims TCP/IP requires content-neutral treatment. The prioritization_reading claims TCP/IP permits differentiated service quality for legitimate network management. The three readings are linked via network.affects_constraints and share the same kernel but have different ε values, different beneficiary/victim sets, and different computed types per seat. Each story instantiates one reading; committer structure (the contest between readings) is routed to omega variables and cs_structure fields rather than split across multiple constraint fields or measurement parameters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
