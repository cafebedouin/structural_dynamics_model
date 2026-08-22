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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: TCP/IP Zero-Rating Reading: Selective Content Exemption
 *   domain: technology/telecommunications/internet_policy
 *
 * SUMMARY:
 *   This constraint instantiates the zero-rating reading of the TCP/IP
 *   interpretation kernel. The reading interprets TCP/IP to permit network
 *   operators to selectively exempt certain content from data caps based on
 *   partnership agreements with content platforms. This is ONE of three
 *   contested readings of the same kernel (end-to-end/neutral interpretation,
 *   differentiated-QoS interpretation, and this zero-rating interpretation).
 *   The reading is characterized by: ISPs gain authority to classify traffic
 *   and grant cap exemptions based on commercial partnerships; incumbent
 *   platforms benefit from cost-of-user-acquisition subsidies; competitive
 *   entrants face data-cap barriers to market entry; data-constrained users
 *   face a bifurcated service landscape where incumbents are subsidized and
 *   competitors are penalized. The constraint's persistence depends on both
 *   technical enforcement (traffic classification, billing system exemptions)
 *   and regulatory non-prohibition (operators claim TCP/IP permits this
 *   reading; regulators debate whether it does).
 *
 * KEY AGENTS:
 *   - network_operators: institutional power, gatekeepers of last-mile infrastructure, agenda-setters for partnership deals
 *   - incumbent_platforms: powerful, beneficiaries of zero-rating exemptions, arbitrage-mobile (can negotiate with any operator)
 *   - independent_content_providers: moderate power, trapped by data-cap disadvantage relative to exempted incumbents
 *   - competitive_entrants: powerless, identity-locked into startup/challenger position, cannot negotiate zero-rating on equal terms
 *   - data_constrained_users: organized power, constrained exit (prepaid/low-income users in monopoly last-mile), structurally beneficiaries of cap exemptions for incumbents but harmed by reduced competition
 *   - regulatory_authorities: institutional power, interpret whether TCP/IP permits this reading or requires neutrality reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, 0.78).
domain_priors:suppression_score(tcp_ip_interpretation__zero_rating_reading, 0.71).
domain_priors:theater_ratio(tcp_ip_interpretation__zero_rating_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__zero_rating_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__zero_rating_reading, "TCP/IP Zero-Rating Reading: Selective Content Exemption").
narrative_ontology:topic_domain(tcp_ip_interpretation__zero_rating_reading, "technology/telecommunications/internet_policy").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__zero_rating_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__zero_rating_reading, '33530142-9d6c-431a-a271-a0fab5f35905').
narrative_ontology:cs_kernel_codification('33530142-9d6c-431a-a271-a0fab5f35905', distributed).
narrative_ontology:cs_authority_grounding('33530142-9d6c-431a-a271-a0fab5f35905', distributed).
narrative_ontology:cs_reading_relation('33530142-9d6c-431a-a271-a0fab5f35905', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('33530142-9d6c-431a-a271-a0fab5f35905', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_axiom('33530142-9d6c-431a-a271-a0fab5f35905', foundational, data_cap_differentiation_permissible).
narrative_ontology:cs_axiom_status(data_cap_differentiation_permissible, holdable).
narrative_ontology:cs_axiom_grounding('33530142-9d6c-431a-a271-a0fab5f35905', data_cap_differentiation_permissible, conventional).
narrative_ontology:cs_axiom('33530142-9d6c-431a-a271-a0fab5f35905', secondary, partnership_based_differentiation_market_efficient).
narrative_ontology:cs_axiom_status(partnership_based_differentiation_market_efficient, holdable).
narrative_ontology:cs_axiom_grounding('33530142-9d6c-431a-a271-a0fab5f35905', partnership_based_differentiation_market_efficient, empirically_contingent).
narrative_ontology:cs_reference_frame('33530142-9d6c-431a-a271-a0fab5f35905', open_protocol_market_differentiation).
narrative_ontology:cs_drift_state('33530142-9d6c-431a-a271-a0fab5f35905', contemporary_market_concentration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('33530142-9d6c-431a-a271-a0fab5f35905', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, incumbent_platforms).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, network_operators).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, independent_content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, competitive_entrants).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, data_constrained_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, data_constrained_users).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, incumbent_platforms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the physical last-mile infrastructure and make zero-rating partnership decisions. They partner with incumbent content platforms, exempting certain traffic from data caps while subject traffic from competitors counts normally. They frame this as network optimization and market innovation; enforce it through billing systems and traffic classification.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, network_operators, agenda_setter,
    institutional, generational, arbitrage, national).

% Negotiate zero-rating deals with operators, gaining competitive advantage by being exempt from data caps that constrain their rivals. They pay operators for the exemption or share revenue from increased usage on the exempted service. They justify the arrangement as enabling affordable access for users with tight data budgets and accelerating adoption of their services.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, incumbent_platforms, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, incumbent_platforms, payer).

% Cannot afford or negotiate zero-rating deals with major operators. Their traffic counts fully against user data caps, making their services less attractive to data-constrained users and creating a cost barrier to competitive entry. They experience the constraint as market access restriction disguised as neutral technical operation.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, independent_content_providers, payer,
    moderate, biographical, constrained, global).

% Attempt to launch video streaming, messaging, or communications services in markets where incumbents hold zero-rating exemptions. They cannot negotiate equivalent deals due to lack of bargaining power or because operators prioritize established partners. The data cap difference makes competitive acquisition economically infeasible.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, competitive_entrants, payer,
    powerless, biographical, trapped, national).

% Have strict data limits due to cost (rural, low-income, prepaid contracts). Zero-rating on incumbent platforms effectively subsidizes their access to those services while making competitive alternatives unaffordable for them. They appear to benefit (lower cost for exempted content) while bearing the indirect cost of reduced competitive pressure and higher pricing on non-exempted services.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, data_constrained_users, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, data_constrained_users, payer).

% The original TCP/IP design enshrined end-to-end principle: the network should be neutral; intelligence and differentiation belong at endpoints, not in the middle. From this epistemic seat, zero-rating arrangements represent a deviation from architectural intent and a shift of control from application layer to network layer.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, network_engineers_and_standards_bodies, observer,
    analytical, civilizational, analytical, universal).

% Investigate whether zero-rating arrangements violate net neutrality obligations, promote fair competition, or constitute anti-competitive leveraging. They interpret the TCP/IP framework's normative constraints and can impose remedies (ban zero-rating, require open negotiation, mandate equivalent access for competitors).
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% In countries where zero-rating is widespread, users face a bifurcated internet: cheap incumbents, expensive competitors. In jurisdictions that ban zero-rating, users retain choice. They have no formal seat in the technical standards bodies that govern the framework and no ability to negotiate directly with operators or platforms.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, global_internet_users, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__zero_rating_reading, incumbent_platforms).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__zero_rating_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches operator network capacity and user demand: by exempting high-traffic services from caps, operators can manage congestion and user satisfaction without provisioning for peak loads on all services simultaneously. Creates partnership incentives for content providers to optimize delivery over the network.
% TRANSFER_FUNCTION: Moves data-cap advantage from competitive entrants to incumbent platforms and from data-constrained users to incumbent platforms via reduced usage constraints. Operators capture fees or revenue-share from partners; independents and entrants pay implicitly through lost market access.
% ABSENT_VOICES: Users in markets with zero-rating have no direct seat but can exit to jurisdictions banning it; in monopoly markets they are locked. Competitive startups and independent content creators are structurally excluded from negotiating zero-rating deals due to bargaining asymmetry and operator preference for proven partners.
% DISAPPEARANCE_RATIONALE: If zero-rating exemptions were prohibited overnight, data-cap impact on service adoption would equalize across platforms; independent and incumbent video services would compete on quality and price rather than on cap-exemption status. Operators would lose partnership revenue but would retain total traffic. Competitive entry would become economically feasible at the application layer.
% FOUNDING_PROBLEM: Mobile data constraints in early 4G deployment (2010–2015): networks operated at capacity, operators sought ways to manage peak loads while enabling growth in high-traffic services (video); zero-rating partnerships were presented as a market mechanism for demand-responsive exemptions rather than requiring blanket capacity increases.
% FOUNDING_PROBLEM_CORROBORATION: Operators and incumbent platforms attest the founding problem is still live: network capacity remains scarce in some markets and times, and zero-rating enables efficient traffic management. Regulators, standards bodies, and competitive advocates attest the founding problem is largely solved (networks are over-provisioned in most developed markets; capacity is commodity-priced, not scarce as presented; zero-rating persists as a market-power extraction mechanism, not a capacity-management tool). Economic analysis and traffic engineering reports from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__zero_rating_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__zero_rating_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__zero_rating_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tcp_ip_interpretation__zero_rating_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__zero_rating_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.78 at interval end) and rising over the interval because zero-rating's primary function has shifted from capacity management (solved by infrastructure investment) to market-power leverage: enabling incumbents to acquire and retain users by making alternatives unaffordable. Suppression is elevated (0.71) because the constraint's persistence requires active enforcement of traffic classification and billing exemptions, plus regulatory resistance (some jurisdictions ban it; others permit it). Theater ratio rises from 0.25 to 0.42, indicating growing performative justification (capacity management rhetoric) as the functional justification (network scarcity) diminishes — a classic piton warning sign masked by a tangled-rope form. The measurement series document this extraction accumulation over the interval: extractiveness rising 0.78-0.62=+16%, theater rising 0.42-0.25=+17%, suppression rising 0.71-0.58=+13%. The constraint computes as tangled_rope because it coordinates (matching capacity to demand) AND extracts asymmetrically (beneficiaries are exempted, victims are penalized for the same bytes); Tangled Rope requires beneficiaries + victims + active enforcement, all present. The measured metrics describe evolution toward piton territory (rising theater, accumulating extraction, persistent enforcement) — the claim/metrics divergence is intentional.
 *
 * PERSPECTIVAL GAP:
 *   Network operators and incumbent platforms frame zero-rating as beneficial market innovation enabling service differentiation and efficient capacity allocation; from the operator's seat the constraint is genuine coordination (match capacity to high-value traffic). Competitive entrants and independent providers frame it as anti-competitive leveraging using network control to foreclose adjacent markets; from their seat the constraint is pure extraction (they must pay implicitly via lost market access). Data-constrained users experience it as a subsidy (free access to incumbents) while losing choice (competitors too expensive). Regulators sit in an observer position debating whether TCP/IP interpretation permits this arrangement; the divergence between readings of the same kernel is EXACTLY what the three sibling constraints measure. The engine computes per-seat type from structural data: operators compute toward beneficiary (d near 0.0); competitors compute toward target (d near 1.0); users compute toward symmetric or slight target (subsidy offset by reduced choice). All three readings are live simultaneously in different jurisdictions and institutional positions; none rules out the others logically, though this reading (zero-rating permissive) creates structural pressure on the neutrality reading (more market power concentration makes the case for mandatory neutrality stronger).
 *
 * DIRECTIONALITY LOGIC:
 *   Network operators are the structural beneficiaries and agenda-setters (they set partnership terms, classify traffic, enforce exemptions; d near 0.0). Incumbent platforms benefit from reduced user-acquisition cost; they pay operators a transaction fee or revenue share (moderate payer, but strong beneficiary — d near 0.2-0.3). Independent content providers and competitive entrants are full targets: they pay implicitly through lost market access and inability to compete on equal terms (d near 1.0). Data-constrained users are a complex seat: they are beneficiaries of exemptions on incumbents (reduced cost), but they also face reduced choice and higher prices on non-exempted services (true cost near 0.5-0.6 — moderately payer-positioned despite appearing as beneficiaries of the exemption structure). Directionality derivation chains from beneficiary/victim declarations + exit options; no overrides needed. The seated divergence is structural: operators and incumbents have arbitrage-grade exit and global scope, making d lower (beneficiary direction); competitive entrants have trapped/identity-locked exit and national/global competitive scope, making d higher (target direction).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope is justified by the presence of BOTH coordination (capacity matching, traffic optimization) AND asymmetric extraction (beneficiaries exempted, victims penalized). The founding problem (network capacity constraints in early 4G) was genuine; the problem status is contested (operators claim still-live; regulators/economists claim solved). The measurement series show extraction accumulating over time (+16% rise) while theater ratio rises (+17%), indicating the constraint's coordinate function is atrophying and being replaced by pure rent extraction. A future state where extraction dominates and capacity scarcity is demonstrably solved would reclassify this toward Snare (pure extraction, cover story). The omega variables document the empirical uncertainties (capacity scarcity, competitive harm magnitude) and the conceptual framing choice (which TCP/IP reading is 'correct'). Mandatrophy is NOT yet resolved (the constraint is still actively enforced as coordination + extraction), but the trajectory suggests resolution toward Snare if the extractive function continues to dominate the coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_capacity_scarcity_resolution,
    'Is network capacity genuinely scarce in the markets where zero-rating is deployed, or has capacity become a commodity that renders the capacity-management justification obsolete?',
    'Technical measurement of network utilization, provisioning costs, and service quality metrics across markets with and without zero-rating; economic analysis of whether zero-rating correlates with capacity constraints or with market-power consolidation.',
    'If capacity is genuinely scarce, zero-rating''s coordination function is real and the Tangled Rope classification holds. If capacity is over-provisioned and cheap, the coordination function has atrophied and the constraint reclassifies toward Snare (pure extraction). The measurement trajectory (rising theater, rising extraction) suggests the latter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_capacity_scarcity_resolution, empirical, 'Whether network capacity scarcity still justifies zero-rating as coordination.').

omega_variable(
    competitive_harm_magnitude_and_antecedent,
    'To what extent do zero-rating exemptions directly cause barriers to competitive entry, versus serving as a marginal advantage that interacts with other market-concentration factors?',
    'Controlled comparison of competitive entry rates and startup survival in markets with zero-rating versus markets with neutrality mandates; econometric isolation of zero-rating''s effect from incumbent platform dominance and network effects.',
    'High direct causation strengthens the Snare interpretation and supports prohibition. Low direct causation with dominance-driven interaction complicates remedy design (zero-rating alone might be insufficient to explain barriers). The classification holds either way, but remedy differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competitive_harm_magnitude_and_antecedent, empirical, 'Magnitude of zero-rating''s role in competitive foreclosure.').

omega_variable(
    tcp_ip_interpretation_kernel_reading_choice,
    'Which reading of TCP/IP is the ''correct'' interpretation: does the protocol''s end-to-end architecture require neutrality (foreclosing zero-rating), permit differentiation as technical tool (coexisting with zero-rating), or permit commercial partnerships (authorizing zero-rating)?',
    'This is conceptual, not empirical: it depends on how one weighs the protocol''s design history (Saltzer/Reed/Clark end-to-end principle), the protocol''s actual technical flexibility (IP allows differentiation), and the policy goals one reads into the architecture. Different authoritative sources (IETF standards, regulatory bodies, courts) offer different readings.',
    'If neutrality reading is adopted as binding interpretation, zero-rating is prohibited. If zero-rating reading is adopted, the constraint persists. If prioritization reading is adopted, zero-rating is permitted under QoS rubric but with tighter oversight. The three readings are presently live simultaneously in different jurisdictions; none is universally foreclosed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tcp_ip_interpretation_kernel_reading_choice, conceptual, 'Interpretive framing of TCP/IP''s normative constraints.').

omega_variable(
    kernel_reading_asymmetric_suppression_mechanism,
    'Is suppression of the neutrality_reading maintained by technical architecture (ISPs CAN differentiate, so they DO), regulatory permission (ISPs ARE ALLOWED to differentiate), or epistemic closure (the TCP/IP framework itself is interpreted to authorize differentiation, making prohibition seem technically impossible)?',
    'Analysis of regulatory action, standards-body guidance, and ISP stated justifications; evidence of whether ISPs treat zero-rating as permitted by the protocol, by regulation, or by technical capability alone. Test epistemic closure by observing whether jurisdictions that explicitly prohibit zero-rating face technical implementation challenges or regulatory confusion.',
    'If suppression is primarily regulatory (not technical), the neutrality_reading can be restored by regulatory action. If it is epistemic (the reading itself naturalizes differentiation), prohibition requires cultural/interpretive shift, not just regulatory change. If it is technical, prohibition requires protocol redesign. This distinctions informs the long-term persistence of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_asymmetric_suppression_mechanism, empirical, 'Mechanism by which zero-rating reading suppresses the neutrality reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__zero_rating_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(tcp__tr_t3, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement(tcp__tr_t6, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement(tcp__tr_t10, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(tcp__tr_t15, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(tcp__tr_t20, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(tcp__be_t3, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 3, 0.66).
narrative_ontology:measurement(tcp__be_t6, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 6, 0.7).
narrative_ontology:measurement(tcp__be_t10, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(tcp__be_t15, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 15, 0.77).
narrative_ontology:measurement(tcp__be_t20, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 20, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(tcp__su_t3, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(tcp__su_t6, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(tcp__su_t10, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(tcp__su_t15, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(tcp__su_t20, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 20, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__zero_rating_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__zero_rating_reading, 0.18).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__prioritization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the tcp_ip_interpretation kernel. The kernel is a contested commitment grounded in the TCP/IP protocol's architecture and interpretation. The zero_rating_reading authorizes selective cap exemptions for partnered content providers; the neutrality_reading requires non-discrimination; the prioritization_reading permits technical quality-of-service differentiation. Each reading produces a distinct constraint with different ε (zero-rating: 0.78; neutrality: estimated ~0.15; prioritization: estimated ~0.45) and different beneficiary/victim structures. They are not observables of a single constraint — they are structurally distinct interpretations of the same kernel, held simultaneously by different jurisdictions and communities. The zero-rating reading influences both siblings: it creates market-power consolidation that strengthens the case for mandatory neutrality, and it demonstrates that technical differentiation CAN be market-power leveraging, complicating the legitimacy of the prioritization reading's QoS justification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tcp_ip_interpretation__zero_rating_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
