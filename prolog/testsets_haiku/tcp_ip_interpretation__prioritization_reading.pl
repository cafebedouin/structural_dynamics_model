% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__prioritization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: tcp_ip_interpretation__prioritization_reading
 *   human_readable: TCP/IP Differentiated Service Quality Authorization (Prioritization Reading)
 *   domain: technology/policy/telecommunications
 *
 * SUMMARY:
 *   The TCP/IP protocol suite does not explicitly forbid or mandate
 *   differentiated service quality — the specification is technically neutral
 *   on QoS. ISP operators, telecommunications regulators, and internet
 *   governance bodies contest how to read this permissiveness. This story
 *   instantiates the prioritization reading: TCP/IP permits ISPs to implement
 *   tiered service quality, negotiate paid fast lanes with content providers,
 *   and implement QoS algorithms that degrade commodity traffic to fund
 *   network investment. The sibling readings (neutrality: strict
 *   non-discrimination; zero-rating: selective exemptions for sponsored
 *   content) decompose the kernel into distinct normative claims with
 *   different extraction profiles and beneficiary structures. This reading
 *   alone carries the prioritization architecture; the others are NOT part of
 *   this story.
 *
 * KEY AGENTS:
 *   - isp_operators: institutional agenda-setters; interpret TCP/IP as permitting differentiation; collect fast-lane revenue
 *   - premium_content_providers: powerful beneficiaries; negotiate prioritization; enjoy reliable delivery
 *   - unfunded_edge_services: powerless payers; no revenue to afford prioritization; experience degraded service
 *   - small_content_providers: moderate payers; constrained exit; face competitive disadvantage
 *   - regulatory_bodies: institutional observers; adjudicate the TCP/IP reading; issue interpretive guidance
 *   - civil_society_advocates: excluded voices; argue against the reading; lack boardroom access
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__prioritization_reading, 0.68).
domain_priors:suppression_score(tcp_ip_interpretation__prioritization_reading, 0.72).
domain_priors:theater_ratio(tcp_ip_interpretation__prioritization_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__prioritization_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__prioritization_reading, "TCP/IP Differentiated Service Quality Authorization (Prioritization Reading)").
narrative_ontology:topic_domain(tcp_ip_interpretation__prioritization_reading, "technology/policy/telecommunications").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__prioritization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__prioritization_reading, 'a102dbc3-4862-4ffc-bcab-17b073363e85').
narrative_ontology:cs_kernel_codification('a102dbc3-4862-4ffc-bcab-17b073363e85', fixed_text).
narrative_ontology:cs_authority_grounding('a102dbc3-4862-4ffc-bcab-17b073363e85', extraction).
narrative_ontology:cs_interpretation_layer_present('a102dbc3-4862-4ffc-bcab-17b073363e85').
narrative_ontology:cs_reading_relation('a102dbc3-4862-4ffc-bcab-17b073363e85', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('a102dbc3-4862-4ffc-bcab-17b073363e85', tcp_ip_interpretation__zero_rating_reading, influences).
narrative_ontology:cs_axiom('a102dbc3-4862-4ffc-bcab-17b073363e85', foundational, isp_capacity_stewardship_authorized).
narrative_ontology:cs_axiom_status(isp_capacity_stewardship_authorized, holdable).
narrative_ontology:cs_axiom_grounding('a102dbc3-4862-4ffc-bcab-17b073363e85', isp_capacity_stewardship_authorized, empirically_contingent).
narrative_ontology:cs_axiom('a102dbc3-4862-4ffc-bcab-17b073363e85', foundational, market_negotiation_legitimate_allocation).
narrative_ontology:cs_axiom_status(market_negotiation_legitimate_allocation, holdable).
narrative_ontology:cs_axiom_grounding('a102dbc3-4862-4ffc-bcab-17b073363e85', market_negotiation_legitimate_allocation, conventional).
narrative_ontology:cs_reference_frame('a102dbc3-4862-4ffc-bcab-17b073363e85', isp_managed_capacity_optimization).
narrative_ontology:cs_drift_state('a102dbc3-4862-4ffc-bcab-17b073363e85', contemporary_net_neutrality_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a102dbc3-4862-4ffc-bcab-17b073363e85', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, isp_operators).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, premium_content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, unfunded_edge_services).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, small_content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, residential_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, residential_users).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__prioritization_reading, network_investment_incentive_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate last-mile infrastructure and interpret TCP/IP rule set as permitting differentiated service quality tiers. They argue that paid fast lanes fund network upgrades, security infrastructure, and backbone capacity. They set QoS parameters, negotiate with content providers, and enforce prioritization rules. They collect revenue from both premium content providers and ISP subscribers paying for higher tiers.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, isp_operators, agenda_setter,
    institutional, generational, arbitrage, regional).

% Large video platforms and data-center operators can afford paid prioritization and gain measurable performance advantage. They negotiate directly with ISPs, pay for fast-lane access, and see their content delivered reliably. Their alternatives include absorbing slower delivery costs or building private networks.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, premium_content_providers, beneficiary,
    powerful, generational, arbitrage, global).

% Non-commercial services, open-source repositories, public health information systems, and educational content without revenue streams cannot afford prioritization fees. They experience degraded delivery quality, user abandonment, and competitive disadvantage relative to funded alternatives. Their exit is exit from the internet entirely.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, unfunded_edge_services, payer,
    powerless, biographical, trapped, global).

% Startups, independent publishers, and regional content providers face binary choice: pay for prioritization they cannot afford or accept degraded performance that erodes their user base. Their capital constraints make the fee a ceiling on market entry. Some relocate to jurisdictions with stronger non-discrimination rules.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, small_content_providers, payer,
    moderate, biographical, constrained, global).

% Pay ISPs for subscriptions and experience service tiers where premium content loads fast while unfunded services buffer. They benefit from improved infrastructure funding and premium service availability, but the cost of access rises and content diversity contracts (unfunded services fail, reducing choice).
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, residential_users, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, residential_users, beneficiary).

% National telecom and competition regulators evaluate whether differentiation violates net neutrality rules or telecommunications law. They read TCP/IP specifications, commission technical analysis, and issue interpretive guidance or prohibitions. Their reading of the TCP/IP kernel shapes whether the prioritization reading persists.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, regulatory_bodies, observer,
    institutional, generational, analytical, national).

% Digital rights organizations and internet freedom advocates argue the prioritization reading violates internet values and democratic infrastructure norms. They lack direct enforcement power but conduct public campaigns, file regulatory comments, and support litigation contesting the reading. They are structurally excluded from ISP boardrooms where the interpretation is operationalized.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, civil_society_advocates, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Differentiating service quality based on content characteristics and investment enables ISPs to match capacity allocation to actual network demand: premium services get reliability guarantees, commodity services use best-effort delivery. This solves the capacity planning problem — unlimited demand cannot be met at uniform quality on finite infrastructure.
% TRANSFER_FUNCTION: Moves revenue from premium content providers and premium-tier residential users to ISP operators in exchange for QoS guarantees and network priority. Simultaneously moves accessibility from unfunded and small content providers to funded and large providers, concentrating audience reach among those who can afford prioritization.
% ABSENT_VOICES: Non-commercial internet services (Wikipedia, Linux distributions, public health databases, open-source communities) would contest the prioritization reading, arguing it degrades the internet's egalitarian infrastructure function. They are absent from ISP strategy conversations because they generate no negotiable revenue and have no institutional seat at governance tables.
% DISAPPEARANCE_RATIONALE: If the prioritization reading were prohibited overnight and strict non-discrimination reinstated, ISPs would lose a major revenue stream (fast-lane fees would collapse), premium content providers would face cost increases (no paid shortcuts), and unfunded services would recover delivery quality. Internet architecture and business models would reorganize around commodity delivery and alternative funding mechanisms.
% FOUNDING_PROBLEM: Network capacity is finite; demand for video streaming and data-intensive services exceeded backbone provisioning. Early internet assumed symmetric, lightweight traffic; modern applications broke that assumption. The problem: how to allocate limited capacity among competing uses without service collapse.
% FOUNDING_PROBLEM_CORROBORATION: ISP operators attest the problem is live and worsening, citing engineering data on peak-load congestion. Network economists and technologists debate whether capacity scarcity is real (ISP argument) or manufactured via under-investment (regulator and advocate argument). Regulatory filings from telecom authorities note the problem statement but dispute the prioritization reading as the appropriate solution.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__prioritization_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__prioritization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__prioritization_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(tcp_ip_interpretation__prioritization_reading, 'none', 1).

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
 *   Extractiveness (0.68) reflects the revenue ISPs collect from premium providers and the accessibility cost imposed on unfunded services — the transfer is decoupled from marginal network cost and depends on ISP market power. Suppression (0.72) is high because the prioritization reading persists through active enforcement: ISP packet inspection, QoS rate-limiting, and contractual terms that exclude rival interpretations. Theater ratio (0.41) shows that roughly two-fifths of the constraint's operational activity is performative: justifying prioritization as necessary for network health, publicizing infrastructure investment, and maintaining the technical framing while extraction is the operative mechanism. The measurement series track the interval from early-stage fast-lane introduction (t=0, lower metrics) to mature tiering (t=25, plateau). Extractiveness plateaus around t=20 because regulatory pressure and market saturation limit further acceleration; suppression plateaus in parallel.
 *
 * PERSPECTIVAL GAP:
 *   From the ISP operator seat: the prioritization reading is legitimate network management grounded in technical necessity and funded by rational market negotiation. From the small provider and unfunded service seats: the same reading is monopolistic extraction disguised as engineering. From the regulatory seat: the question is whether the TCP/IP kernel permits or forbids the prioritization reading — the answering authority adjudicates which interpretation prevails. The engine computes these perspectives from the structural data: ISP beneficiary status, small-provider victim status, constrained exits, and regulatory power to enforce competing readings.
 *
 * DIRECTIONALITY LOGIC:
 *   ISP operators are the structural beneficiaries (d near 0.0): they set the reading, collect the revenue, maintain arbitrage exit options (can switch regulatory jurisdictions or business models). Premium content providers are secondary beneficiaries (d near 0.15): they benefit from prioritization but depend on ISPs for implementation. Unfunded edge services are full targets (d near 1.0): powerless, trapped, no revenue to negotiate, directly harmed by degradation. Small content providers are partial targets (d near 0.75): moderate power, constrained exit, asymmetric harm. Residential users are near symmetric (d near 0.5): genuine infrastructure benefit from investment, but indirect cost from reduced content diversity and higher subscription tiers. Regulatory bodies sit at d=0.5 (analytical seat, no extraction collected or imposed).
 *
 * MANDATROPHY ANALYSIS:
 *   The prioritization reading instantiates a tangled rope: it coordinates real network capacity allocation (coordination function) AND extracts revenue from content providers and disadvantages unfunded services (asymmetric extraction). The coordination function — matching service quality to demand — is genuine but not exhaustive: bandwidth scarcity could be solved via alternative mechanisms (congestion pricing for all users uniformly, universal rate caps, public investment in capacity). The extraction component (revenue concentration with ISPs, accessibility closure for unfunded services) does NOT follow logically from the coordination function. A Tangled Rope diagnosis is correct because the arrangement requires active enforcement (suppression=0.72) and cannot persist as pure coordination — unfunded services would resist and small providers would lobby for alternative readings without ISP policing and regulatory gatekeeping.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_scarcity_reality,
    'Is network capacity scarcity real, or is it constructed through under-investment and artificial rate-limiting to justify the prioritization reading?',
    'Comparative engineering analysis of capacity provisioning in jurisdictions that prohibited differentiation (EU, India) versus jurisdictions that permitted it (USA, South Korea); examination of ISP investment patterns in relation to revenue from fast lanes.',
    'If scarcity is real, the prioritization reading solves a genuine coordination problem and extraction is a necessary cost of network maintenance. If constructed, the reading is pure extraction disguised as technical necessity and should be classified as snare rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_scarcity_reality, empirical, 'Whether network scarcity justifies or merely rationalizes differentiation.').

omega_variable(
    alternative_capacity_solutions,
    'Are there non-discriminatory mechanisms (uniform congestion pricing, public capacity investment, architectural redesign) that solve the capacity allocation problem without concentration of control and revenue with ISPs?',
    'Technical and economic analysis of alternative architectures; regulatory experiments in jurisdictions adopting neutrality rules (were capacity problems solved differently or did service degrade).',
    'If alternatives exist and succeed, the prioritization reading''s claim to necessity collapses; extraction becomes the motive force and classification shifts toward snare. If alternatives fail technically or economically, the reading''s necessity claim holds and tangled_rope classification is appropriate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_capacity_solutions, conceptual, 'Whether differentiation is the only solution to capacity management or one choice among structurally equivalent alternatives.').

omega_variable(
    reading_stability_under_regulatory_pressure,
    'Will the prioritization reading persist if regulatory or legislative pressure strengthens, or is it contingent on ISP market power and weak enforcement of alternatives?',
    'Observation of jurisdictions that have shifted from permitting to prohibiting prioritization (or vice versa), and analysis of what changed in ISP behavior when the regulatory reading shifted.',
    'If the reading persists only under weak regulatory conditions, it is contingent institutional power rather than stable technical fact. If it persists across regulatory shifts because network engineering supports it, the reading is more robust to challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_stability_under_regulatory_pressure, empirical, 'Whether the prioritization reading''s persistence depends on ISP power or on technical necessity.').

omega_variable(
    suppression_mechanism_locus,
    'Is the suppression (0.72) primarily structural (market power, lack of viable alternatives) or internalized (unfunded services accept degradation as legitimate; users believe prioritization is necessary)?',
    'Post-regulation trajectory: if suppression persists after jurisdictions prohibit differentiation (unfunded services remain degraded, users resist cost reductions), suppression is partly internalized; if suppression evaporates when prohibited, it was primarily structural.',
    'If internalized, unfunded services carry the suppression across regulatory contexts and may not recover after prohibition. If structural, prohibition removes the suppression mechanism and recovery is possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_locus, empirical, 'Whether measured suppression is structural market power or internalized acceptance of hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__prioritization_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__prioritization_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(tcp__tr_t0, observed).
narrative_ontology:measurement(tcp__tr_t5, tcp_ip_interpretation__prioritization_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement_basis(tcp__tr_t5, observed).
narrative_ontology:measurement(tcp__tr_t10, tcp_ip_interpretation__prioritization_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement_basis(tcp__tr_t10, observed).
narrative_ontology:measurement(tcp__tr_t15, tcp_ip_interpretation__prioritization_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(tcp__tr_t15, observed).
narrative_ontology:measurement(tcp__tr_t20, tcp_ip_interpretation__prioritization_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(tcp__tr_t20, observed).
narrative_ontology:measurement(tcp__tr_t25, tcp_ip_interpretation__prioritization_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(tcp__tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(tcp__be_t0, observed).
narrative_ontology:measurement(tcp__be_t5, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(tcp__be_t5, observed).
narrative_ontology:measurement(tcp__be_t10, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(tcp__be_t10, observed).
narrative_ontology:measurement(tcp__be_t15, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(tcp__be_t15, observed).
narrative_ontology:measurement(tcp__be_t20, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(tcp__be_t20, observed).
narrative_ontology:measurement(tcp__be_t25, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(tcp__be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(tcp__su_t0, observed).
narrative_ontology:measurement(tcp__su_t5, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(tcp__su_t5, observed).
narrative_ontology:measurement(tcp__su_t10, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(tcp__su_t10, observed).
narrative_ontology:measurement(tcp__su_t15, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(tcp__su_t15, observed).
narrative_ontology:measurement(tcp__su_t20, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(tcp__su_t20, observed).
narrative_ontology:measurement(tcp__su_t25, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(tcp__su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__prioritization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__prioritization_reading, 0.18).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% The tcp_ip_interpretation kernel decomposes into three structurally distinct constraint stories. The prioritization_reading (this file) asserts TCP/IP permits tiered quality and ISPs should implement fast lanes — ε=0.68, tangled_rope, beneficiaries: ISP operators and premium providers, victims: unfunded services and small providers. The neutrality_reading asserts strict non-discrimination — ε close to zero (coordination only), rope, beneficiaries: universal internet access. The zero_rating_reading asserts selective exemptions for sponsored content — ε intermediate (partial extraction through selective exemption), tangled_rope. Each reading instantiates a different constraint because the beneficiary sets, extraction targets, and enforcement mechanisms are distinct. The kernel itself (TCP/IP specification) is technically neutral; the contest is over interpretation. The three stories are linked via network.affects_constraints because regulatory adoption of one reading creates downstream pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tcp_ip_interpretation__prioritization_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
