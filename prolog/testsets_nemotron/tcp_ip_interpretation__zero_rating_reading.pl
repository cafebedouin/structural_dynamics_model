% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__zero_rating_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Zero-Rating Sponsored Data Exemptions under TCP/IP Interpretation
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   This constraint story captures the zero-rating reading of the contested
 *   TCP/IP interpretation kernel. Under this reading, the protocol suite's
 *   silence on application-layer discrimination is read as permission for
 *   ISPs to exempt sponsored content from data caps. The arrangement presents
 *   as a coordination mechanism (traffic management, consumer savings) but
 *   operates with substantial asymmetric extraction: incumbent platforms pay
 *   for privileged access, competitive entrants are structurally
 *   disadvantaged, and users on capped plans face distorted choice sets. The
 *   claim/metric gap is deliberate — the reading claims coordination
 *   (rope/tangled_rope framing) while the authored metrics describe a
 *   substantially extractive, actively enforced structure that advantages
 *   powerful incumbents.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, 0.68).
domain_priors:suppression_score(tcp_ip_interpretation__zero_rating_reading, 0.72).
domain_priors:theater_ratio(tcp_ip_interpretation__zero_rating_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__zero_rating_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__zero_rating_reading, "Zero-Rating Sponsored Data Exemptions under TCP/IP Interpretation").
narrative_ontology:topic_domain(tcp_ip_interpretation__zero_rating_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__zero_rating_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__zero_rating_reading, '167f1cb6-90fb-4fb3-81e8-3368dc90190c').
narrative_ontology:cs_kernel_codification('167f1cb6-90fb-4fb3-81e8-3368dc90190c', distributed).
narrative_ontology:cs_authority_grounding('167f1cb6-90fb-4fb3-81e8-3368dc90190c', practice).
narrative_ontology:cs_interpretation_layer_present('167f1cb6-90fb-4fb3-81e8-3368dc90190c').
narrative_ontology:cs_reading_relation('167f1cb6-90fb-4fb3-81e8-3368dc90190c', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('167f1cb6-90fb-4fb3-81e8-3368dc90190c', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_axiom('167f1cb6-90fb-4fb3-81e8-3368dc90190c', foundational, protocol_silence_permits_commercial_discrimination).
narrative_ontology:cs_axiom_status(protocol_silence_permits_commercial_discrimination, holdable).
narrative_ontology:cs_axiom_grounding('167f1cb6-90fb-4fb3-81e8-3368dc90190c', protocol_silence_permits_commercial_discrimination, conventional).
narrative_ontology:cs_axiom('167f1cb6-90fb-4fb3-81e8-3368dc90190c', secondary, sponsored_data_enables_consumer_savings).
narrative_ontology:cs_axiom_status(sponsored_data_enables_consumer_savings, holdable).
narrative_ontology:cs_axiom_grounding('167f1cb6-90fb-4fb3-81e8-3368dc90190c', sponsored_data_enables_consumer_savings, instrumental).
narrative_ontology:cs_reference_frame('167f1cb6-90fb-4fb3-81e8-3368dc90190c', early_mobile_broadband_expansion).
narrative_ontology:cs_drift_state('167f1cb6-90fb-4fb3-81e8-3368dc90190c', post_capacity_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('167f1cb6-90fb-4fb3-81e8-3368dc90190c', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, incumbent_platforms).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, isp_carriers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, competitive_entrants).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, end_users_non_partnered).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, content_providers_non_partnered).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__zero_rating_reading, network_management_flexibility_doctrine).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__zero_rating_reading, sponsored_data_as_business_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the network infrastructure and administer zero-rating programs. Negotiate sponsorship deals with content providers, collect sponsorship fees (directly or via data plan upsells), and enforce the technical exemptions at the packet-inspection layer. Justify the arrangement as network management flexibility and consumer choice.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, isp_carriers, agenda_setter,
    institutional, generational, arbitrage, national).

% Large, established content and application providers (video streaming, social media, messaging) that can afford sponsorship fees. Gain competitive advantage: their traffic is zero-rated while rivals' traffic counts against user caps. This entrenches market position and raises entry barriers for new competitors who cannot match sponsorship costs.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, incumbent_platforms, beneficiary,
    powerful, biographical, mobile, global).

% New or smaller content providers, startups, and niche platforms. Cannot afford zero-rating sponsorship fees at scale. Their traffic consumes user data caps while incumbent traffic does not, creating a structural cost disadvantage that distorts competition on the merits. Exit means abandoning the market or accepting the disadvantage.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, competitive_entrants, payer,
    moderate, biographical, constrained, global).

% Mid-sized content providers and independent creators who lack sponsorship deals. Their audiences face data-cap penalties for consuming their content, while zero-rated competitors do not. They bear the extraction indirectly through audience attrition and reduced engagement.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, content_providers_non_partnered, payer,
    organized, biographical, constrained, global).

% Subscribers on capped data plans whose usage of non-zero-rated services consumes their allowance. They pay the opportunity cost: either restrict usage to zero-rated services (reducing choice) or pay overage fees. In markets with limited ISP competition, switching providers is not feasible.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, end_users_non_partnered, payer,
    powerless, immediate, trapped, local).

% Represent user interests in regulatory proceedings. Argue zero-rating violates non-discrimination principles and harms competition. Their participation is permitted in rulemaking but their preferred outcome (prohibition) is structurally opposed by the agenda-setters who benefit from the arrangement.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, consumer_advocacy_groups, excluded,
    organized, biographical, constrained, national).

% FCC, Ofcom, BEREC, and national telecom regulators. Oversee whether zero-rating complies with net neutrality or open internet rules. Can impose remedies (transparency requirements, case-by-case review, outright bans) but face industry lobbying and jurisdictional fragmentation. Their enforcement posture shifts with political cycles.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows ISPs to manage network congestion and offer differentiated service tiers by exempting sponsored traffic from data caps, theoretically lowering consumer cost for popular services and enabling new business models for content delivery.
% TRANSFER_FUNCTION: Moves competitive advantage and audience access from non-partnered content providers to partnered incumbents; moves sponsorship revenue from platforms to ISPs (or from users to ISPs via plan upsells); moves data-cap burden from zero-rated traffic to all other traffic.
% ABSENT_VOICES: Small content creators, niche platforms, and users in monopolistic ISP markets who cannot switch providers. They would object to the competitive distortion and reduced choice but are not represented in the sponsorship negotiation between ISPs and incumbent platforms.
% DISAPPEARANCE_RATIONALE: If zero-rating exemptions vanished overnight, ISPs would lose a sponsorship revenue stream and a network-management tool; incumbent platforms would lose their data-cap advantage and face renewed competitive pressure; competitive entrants would gain a more level playing field; users would see uniform data-metering but potentially higher baseline costs if ISPs recoup lost sponsorship revenue.
% FOUNDING_PROBLEM: Growing mobile data demand strained network capacity; ISPs sought traffic-management tools and new revenue streams; large content providers sought guaranteed quality of experience for their users; consumers faced rising data costs.
% FOUNDING_PROBLEM_CORROBORATION: ISPs and incumbent platforms attest the founding problem (congestion, cost, quality) remains live and zero-rating is a valid response. Consumer advocates, competition authorities, and independent economic analyses (e.g., BEREC 2016 guidelines, FCC 2015 Open Internet Order record, academic studies on zero-rating competitive effects) attest the founding problem has been substantially addressed by capacity expansion and that the arrangement now functions primarily as rent extraction and competitive foreclosure.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__zero_rating_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__zero_rating_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__zero_rating_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(tcp_ip_interpretation__zero_rating_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__zero_rating_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is substantial because the sponsorship model transfers competitive advantage and revenue to incumbents and ISPs while the coordination justification (congestion management) has weakened as capacity expanded. Suppression (0.72) is high because the constraint's persistence depends on active enforcement: deep-packet inspection to identify zero-rated traffic, contractual exclusivity terms, and regulatory capture that prevents prohibition. Theater ratio (0.38) reflects that the network-management justification is real but declining in proportion to the rent-extraction function. The measurement series uses a shared time grid (2010-2025, 6 points) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seats (ISPs, incumbents) experience this as a legitimate business model and network management tool — genuine coordination from their position. The payer seats (entrants, non-partnered providers, capped users) experience the same structure as enforced extraction that distorts competition and choice. The engine computes this per-seat divergence from the structural data; the authored claim (tangled_rope) acknowledges both functions exist simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   ISP carriers are structural beneficiaries (collect sponsorship revenue, control the rules, arbitrage-grade exit — d near beneficiary end). Incumbent platforms are beneficiaries (gain competitive moat, mobile exit — d near beneficiary end). Competitive entrants and non-partnered content providers are targets (bear competitive disadvantage, constrained exit — d near target end). End users on capped plans are targets (bear opportunity cost, trapped exit in monopoly markets — d near target end). Consumer advocates are excluded (would object, constrained exit). Regulators are observers (analytical seat).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (congestion, cost, quality) is contested: ISPs and incumbents say it persists; independent evidence says capacity expansion has largely solved it while the arrangement persists as rent extraction. This is a classic mandatrophy signature — a coordination arrangement whose founding justification has atrophied but whose enforcement machinery remains active and expands. The theater trajectory (rising) and extractiveness trajectory (rising) confirm the drift from coordination toward extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_structure,
    'This constraint is one reading (zero_rating_reading) of the contested tcp_ip_interpretation kernel. What would the sibling readings change structurally?',
    'Comparative analysis of the three readings'' beneficiary/victim structures, extractiveness profiles, and enforcement requirements. The kernel context declares the expected deltas: ISPs authorized to partner with content providers for data cap exemptions; incumbent platforms advantaged; competitive entry barriers raised.',
    'If the neutrality_reading were instantiated instead, extractiveness would be near-zero (mountain/rope), beneficiaries would be end-users generally, victims would be absent. If prioritization_reading were instantiated, extractiveness would be moderate (technical QoS management), beneficiaries would be latency-sensitive applications, victims would be best-effort traffic. The reading choice IS the constraint identity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_structure, conceptual, 'Committee frame: this constraint is one reading of a contested kernel; sibling readings instantiate different constraints with different ε and structural profiles').

omega_variable(
    congestion_justification_validity,
    'Does zero-rating materially reduce network congestion, or is the congestion-management justification a cover for rent extraction?',
    'Empirical analysis of traffic patterns in jurisdictions with and without zero-rating; ISP internal capacity-planning documents; comparative network performance metrics during peak hours with zero-rated vs. non-zero-rated traffic.',
    'If congestion reduction is negligible, the coordination function is largely theatrical and the constraint trends toward snare. If substantial, the tangled_rope classification is sustained but the extraction/coordination boundary remains contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congestion_justification_validity, empirical, 'Whether the stated coordination function (congestion management) materially operates or is pretextual').

omega_variable(
    competitive_foreclosure_magnitude,
    'How large is the competitive disadvantage imposed on non-zero-rated entrants, and does it constitute foreclosure or merely distortion?',
    'Market-entry rates and survival analysis for content providers in zero-rating vs. non-zero-rating markets; user engagement differentials between zero-rated and metered competitors; econometric estimation of sponsorship cost as barrier to entry.',
    'If foreclosure is demonstrated (entrants systematically fail or cannot enter), the extraction is structural and the constraint''s persistence is actively harmful to market structure. If merely distortionary, the constraint is harmful but potentially remediable by transparency or non-discrimination rules.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competitive_foreclosure_magnitude, empirical, 'Magnitude and nature of competitive harm to non-partnered content providers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__zero_rating_reading, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp_ip_interpretation__zero_rating_reading_tr_t2010, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(tcp_ip_interpretation__zero_rating_reading_tr_t2013, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 2013, 0.18).
narrative_ontology:measurement(tcp_ip_interpretation__zero_rating_reading_tr_t2016, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 2016, 0.25).
narrative_ontology:measurement(tcp_ip_interpretation__zero_rating_reading_tr_t2019, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 2019, 0.31).
narrative_ontology:measurement(tcp_ip_interpretation__zero_rating_reading_tr_t2022, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 2022, 0.35).
narrative_ontology:measurement(tcp_ip_interpretation__zero_rating_reading_tr_t2025, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(tcp_ip_interpretation__zero_rating_reading_be_t2010, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement(tcp_ip_interpretation__zero_rating_reading_be_t2013, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 2013, 0.42).
narrative_ontology:measurement(tcp_ip_interpretation__zero_rating_reading_be_t2016, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 2016, 0.55).
narrative_ontology:measurement(tcp_ip_interpretation__zero_rating_reading_be_t2019, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 2019, 0.62).
narrative_ontology:measurement(tcp_ip_interpretation__zero_rating_reading_be_t2022, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 2022, 0.66).
narrative_ontology:measurement(tcp_ip_interpretation__zero_rating_reading_be_t2025, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tcp_ip_interpretation__zero_rating_reading_su_t2010, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 2010, 0.45).
narrative_ontology:measurement(tcp_ip_interpretation__zero_rating_reading_su_t2013, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 2013, 0.52).
narrative_ontology:measurement(tcp_ip_interpretation__zero_rating_reading_su_t2016, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 2016, 0.62).
narrative_ontology:measurement(tcp_ip_interpretation__zero_rating_reading_su_t2019, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 2019, 0.68).
narrative_ontology:measurement(tcp_ip_interpretation__zero_rating_reading_su_t2022, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 2022, 0.7).
narrative_ontology:measurement(tcp_ip_interpretation__zero_rating_reading_su_t2025, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__zero_rating_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__zero_rating_reading, 0.18).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__prioritization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the tcp_ip_interpretation constraint family (kernel: tcp_ip_interpretation). The three readings — neutrality_reading, prioritization_reading, zero_rating_reading — decompose the colloquial label 'TCP/IP interpretation' into structurally distinct claims with different ε values, beneficiary/victim structures, and enforcement profiles. They are linked via affects_constraints to enable contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tcp_ip_interpretation__zero_rating_reading, institutional, 0.15).
constraint_indexing:directionality_override(tcp_ip_interpretation__zero_rating_reading, powerful, 0.2).
constraint_indexing:directionality_override(tcp_ip_interpretation__zero_rating_reading, moderate, 0.75).
constraint_indexing:directionality_override(tcp_ip_interpretation__zero_rating_reading, organized, 0.7).
constraint_indexing:directionality_override(tcp_ip_interpretation__zero_rating_reading, powerless, 0.9).
constraint_indexing:directionality_override(tcp_ip_interpretation__zero_rating_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
