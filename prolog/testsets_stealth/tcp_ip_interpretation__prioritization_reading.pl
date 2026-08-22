% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__prioritization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: tcp_ip_interpretation__prioritization_reading
 *   human_readable: Paid Prioritization Authorization Reading of TCP/IP
 *   domain: technology_governance/internet_policy
 *
 * SUMMARY:
 *   This story instantiates the prioritization_reading of the
 *   tcp_ip_interpretation kernel: the claim that the TCP/IP design permits
 *   differentiated service quality as ordinary network management, and
 *   therefore that broadband operators may sell assured-performance tiers to
 *   edge providers able to pay. The standing arrangement under contest — the
 *   permissive regime in which paid prioritization operates — is assessed by
 *   this reading's own lights: the reading credits the arrangement's genuine
 *   traffic-engineering function while conceding, in its own expected
 *   structural delta, that unfunded edge services are disadvantaged. Epsilon
 *   is authored for that standing arrangement, never for the
 *   neutrality-compliant alternative this reading's opponents would install.
 *   Per the epsilon-invariance principle this is one of three linked stories
 *   decomposing the colloquial 'net neutrality' label: the kernel (what the
 *   TCP/IP design entails for traffic handling) is read differently by
 *   neutrality_reading (non-discrimination imperative), this
 *   prioritization_reading (operator discretion), and zero_rating_reading
 *   (selective sponsorship exemptions). Each is a separate constraint with
 *   its own epsilon, beneficiaries, and victims, linked via
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   broadband_isps: Agenda-setter and receipt seat
 *   (institutional/constrained) — administers tiering, collects the fees -
 *   large_edge_platforms: Dual-positioned participant (powerful/arbitrage) —
 *   pays the toll and collects the relative advantage -
 *   unfunded_edge_services: Primary target (moderate/trapped) — bears
 *   degraded default-tier performance - small_content_startups: Entrant
 *   target (powerless/trapped) — faces incumbents holding purchased priority
 *   - end_users: Diffuse payer-beneficiary (moderate/constrained) — buys
 *   tiers, bears pass-through - open_internet_advocates: Excluded voice
 *   (organized/constrained) — contests the reading outside the bargaining
 *   table - telecom_regulators: Analytical observer
 *   (institutional/analytical) — adjudicates management-versus-discrimination
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__prioritization_reading, 0.52).
domain_priors:suppression_score(tcp_ip_interpretation__prioritization_reading, 0.5).
domain_priors:theater_ratio(tcp_ip_interpretation__prioritization_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__prioritization_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__prioritization_reading, "Paid Prioritization Authorization Reading of TCP/IP").
narrative_ontology:topic_domain(tcp_ip_interpretation__prioritization_reading, "technology_governance/internet_policy").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__prioritization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__prioritization_reading, 'e1b59cd2-cf6d-4f09-9a65-8b36d8d3a209').
narrative_ontology:cs_kernel_codification('e1b59cd2-cf6d-4f09-9a65-8b36d8d3a209', fixed_text).
narrative_ontology:cs_authority_grounding('e1b59cd2-cf6d-4f09-9a65-8b36d8d3a209', practice).
narrative_ontology:cs_interpretation_layer_present('e1b59cd2-cf6d-4f09-9a65-8b36d8d3a209').
narrative_ontology:cs_reading_relation('e1b59cd2-cf6d-4f09-9a65-8b36d8d3a209', tcp_ip_interpretation__neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('e1b59cd2-cf6d-4f09-9a65-8b36d8d3a209', tcp_ip_interpretation__zero_rating_reading, influences).
narrative_ontology:cs_axiom('e1b59cd2-cf6d-4f09-9a65-8b36d8d3a209', foundational, differentiation_as_engineering_prerogative).
narrative_ontology:cs_axiom_status(differentiation_as_engineering_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('e1b59cd2-cf6d-4f09-9a65-8b36d8d3a209', differentiation_as_engineering_prerogative, instrumental).
narrative_ontology:cs_axiom('e1b59cd2-cf6d-4f09-9a65-8b36d8d3a209', foundational, capacity_investment_requires_pricing_flexibility).
narrative_ontology:cs_axiom_status(capacity_investment_requires_pricing_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('e1b59cd2-cf6d-4f09-9a65-8b36d8d3a209', capacity_investment_requires_pricing_flexibility, empirically_contingent).
narrative_ontology:cs_reference_frame('e1b59cd2-cf6d-4f09-9a65-8b36d8d3a209', operator_management_prerogative).
narrative_ontology:cs_drift_state('e1b59cd2-cf6d-4f09-9a65-8b36d8d3a209', contemporary_neutrality_regulation_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e1b59cd2-cf6d-4f09-9a65-8b36d8d3a209', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, broadband_isps).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, large_edge_platforms).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, unfunded_edge_services).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, small_content_startups).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, end_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, end_users).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, large_edge_platforms).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__prioritization_reading, network_engineering_autonomy_doctrine).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__prioritization_reading, tiered_service_investment_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate access networks, define which traffic classes receive queue priority, and sell assured-performance tiers and enhanced interconnection to edge providers. Enforce tiering through deep packet inspection, contract terms, and peering leverage. Collect the prioritization fees directly. Exit would mean abandoning their own physical plant, so they are bound to the arrangement they administer.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, broadband_isps, agenda_setter,
    institutional, generational, constrained, national).

% Purchase fast-lane and paid-peering arrangements that guarantee performance for their traffic. Gain a durable relative advantage over rivals that do not pay, while bearing the toll itself. Can arbitrage between providers, run private CDNs, and shift traffic across interconnection points, giving them more room than smaller edge firms.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, large_edge_platforms, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, large_edge_platforms, payer).

% Deliver over the default best-efforts tier and absorb degraded latency and throughput whenever funded competitors occupy the priority queues. Paying the prioritization fee is often the only way to restore parity, and switching access ISPs does not escape tiering because each access network runs its own scheme.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, unfunded_edge_services, payer,
    moderate, biographical, trapped, global).

% Enter markets where incumbents already hold purchased priority. Face a performance gap at launch, no leverage in interconnection negotiations, and a fee schedule sized for established firms. Their realistic choices are paying for tiers they cannot afford or accepting second-class delivery.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, small_content_startups, payer,
    powerless, immediate, trapped, global).

% Buy broadband tiers and receive better real-time application quality on premium products. Bear indirect costs where edge providers pass prioritization fees into prices, and face reduced service diversity where unfunded entrants fail. Local ISP competition is thin, so switching is costly and traffic-handling terms are opaque to them.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, end_users, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, end_users, beneficiary).

% Campaign against paid prioritization through petitions, comment dockets, and litigation. Have no seat where interconnection contracts and tiering terms are actually negotiated, and must reach the arrangement indirectly through regulators and courts.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, open_internet_advocates, excluded,
    organized, generational, constrained, global).

% Adjudicate whether differentiated service quality is lawful network management or unlawful discrimination. Take testimony from the other seats, commission economic and engineering analysis, and issue rules that successive administrations and courts have reversed, vacated, and reinstated.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, telecom_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__prioritization_reading, broadband_isps).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__prioritization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates finite access-network capacity among heterogeneous traffic classes: latency-sensitive flows such as voice, telemedicine, and interactive applications receive queue priority while bulk transfers defer. Solves congestion management and quality-of-service assurance that undifferentiated best-efforts delivery handles poorly at peak load.
% TRANSFER_FUNCTION: Moves fee income from edge service providers and premium-tier subscribers to broadband ISPs in exchange for assured performance; moves effective network performance toward funded traffic and away from unfunded traffic riding the same links.
% ABSENT_VOICES: Unfunded edge services and not-yet-existing startups have no seat where interconnection and tiering terms are set; open-internet advocates stand outside the commercial negotiation and reach the arrangement only through regulators; end users appear solely as aggregate demand, never as parties to traffic-handling terms.
% DISAPPEARANCE_RATIONALE: If the authorization vanished overnight, ISPs would revert to uniform best-efforts handling, existing fast-lane contracts would be void, edge providers would re-optimize around equal treatment, and the quality-assurance market built on tiering would dissolve into congestion pricing or capacity-expansion substitutes.
% FOUNDING_PROBLEM: Best-efforts IP delivery cannot assure latency or jitter for real-time applications, and flat-rate funding gave network operators no revenue signal for capacity expansion as traffic volumes grew and application requirements diverged.
% FOUNDING_PROBLEM_CORROBORATION: Independent queueing-theory and network-measurement literature corroborates that congestion and heterogeneous application requirements are real, ongoing problems. However, transport economists and neutrality scholarship outside the ISP beneficiary set dispute that paid prioritization is a necessary or efficient remedy — corroboration covers the founding problem, not the arrangement sold as its solution.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__prioritization_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__prioritization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__prioritization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tcp_ip_interpretation__prioritization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__prioritization_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate (0.52): the reading's own lights concede the unfunded-edge disadvantage, and prioritization prices sit above measurable queue-management cost, but part of the charge recovers real capacity and engineering. Suppression (0.50) is structural — deep packet inspection, contract terms, and interconnection leverage sustain tiering — while alternatives (CDNs, multi-homing, protocol-level optimization) remain partially available, so accessibility collapse is incomplete (0.48). Resistance is high (0.68): the neutrality movement, litigation, and recurring rulemaking cycles contest the reading continuously. Theater is low-moderate (0.30): most traffic-management activity performs real queuing work; a growing minority reframes ordinary capacity as premium product. All three temporal series share one seven-point grid (T0 approximates the 1998 differentiated-services architecture; T27 approximates 2025). Base extractiveness dips at T18 (approx. 2016) when the US Open Internet Order barred paid prioritization, then rebounds post-repeal — a regulatory oscillation, not noise: the ban phase compressed domestic extraction while the surrounding litigation and lobbying apparatus kept building, which is why suppression_requirement rises monotonically through the same window. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine, via directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as coordination it built and maintains: queues managed, latency-sensitive applications served, investment signaled. The trapped payer seats experience the same structure as a two-tier market they never consented to, where exit from the default tier is priced and exit from the market is fatal. Large edge platforms straddle: they pay the toll and collect the relative advantage, so their computed position should sit nearer symmetric than either pole. The regulator seat sees a classification that flips with administrations. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   broadband_isps are declared beneficiaries and the receipt seat: derivation places them near the beneficiary end. unfunded_edge_services and small_content_startups are declared victims with trapped exit: derivation places them near the full-target end. end_users carry both roles — they buy tiers and bear pass-through — placing them near symmetric. large_edge_platforms are declared beneficiaries but are structurally dual: they fund the very tiering they profit from, so a directionality override moves the powerful atom to 0.38 rather than letting the beneficiary declaration alone pull them toward the subsidy end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — best-efforts delivery cannot assure latency, and flat-rate funding gives no capacity signal — remains live, corroborated by independent network-measurement literature, so this is not a mandatrophy case. The tangled_rope claim guards against both mislabelings: a pure-rope verdict would erase the priced disadvantage the reading itself concedes; a pure-snare verdict would deny the real queuing and quality-of-service function that latency-sensitive traffic demonstrably receives. The low theater ratio rules out piton drift for now. The watch item is Goodhart drift: if 'network management' hardens into the marketing label for scarcity monetization, theater_ratio climbs, the coordination half atrophies, and the arrangement slides toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading of the tcp_ip_interpretation kernel; how would the sibling readings restructure the classification of the same fast-lane arrangements?',
    'Generate the sibling stories (neutrality_reading, zero_rating_reading) and compare victim sets, epsilon, and computed types across the family.',
    'Under neutrality_reading the same paid fast lanes instantiate unlawful discrimination with materially higher epsilon and expanded victim sets; under zero_rating_reading sponsored exemptions become the contested surface. Every classification in this file is conditional on the prioritization premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Committer contingency: classification holds only within the prioritization reading of the kernel.').

omega_variable(
    management_rent_boundary,
    'Where does legitimate traffic engineering end and rent extraction begin — what fraction of prioritization charges reflects marginal capacity and queue-management cost versus monopoly position?',
    'Cost-accounting disclosure in interconnection disputes; comparison of prioritization prices against measured marginal quality-of-service provisioning costs.',
    'A wide price-to-cost gap shifts effective extraction upward and pushes the arrangement toward snare; a narrow gap supports the coordination-dominant reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(management_rent_boundary, empirical, 'Boundary between engineering cost recovery and positional rent in tier pricing.').

omega_variable(
    investment_incentive_efficacy,
    'Does tiering revenue actually finance incremental network capacity, or does it substitute for capacity investment by monetizing engineered scarcity?',
    'Panel analysis of ISP capital expenditure before and after prioritization product launches, controlling for subscriber growth and regulatory regime.',
    'If substitution dominates, the capacity-investment axiom loses its empirical footing and the reading''s foundational warrant erodes (axiom_overriding drift); if additive, the coordination claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investment_incentive_efficacy, empirical, 'Empirical footing of the investment-incentive axiom underlying this reading.').

omega_variable(
    design_intent_authority,
    'Who holds interpretive authority over what the TCP/IP design requires — the original engineering documents and their authors, the accumulated practice of network operators, or regulatory reinterpretation?',
    'Doctrinal analysis of how courts and agencies weigh RFC history, operator practice, and statutory text in discrimination cases.',
    'Locates the disagreement with neutrality_reading precisely: the sibling treats the end-to-end principle as constitutive of the design; this reading treats design intent as silent on commercial traffic handling. Whichever seat wins authority reshapes both readings'' legitimacy conditions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(design_intent_authority, conceptual, 'Where the kernel contest is located: interpretive authority over design intent.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel the protocol specification itself, or the engineering-autonomy legitimacy claim layered above it on which operator practice depends?',
    'Test classification stability under the alternative framing: if the kernel is the autonomy claim, drift analysis keys to erosion of that claim rather than to the RFC text.',
    'Under the layered framing, repudiation_pressure intensifies because neutrality regulation attacks the autonomy claim directly, and this reading''s drift magnitude moves toward severe; under the text framing the current substantial rating stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Two coherent framings of the kernel yield different drift profiles for this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__prioritization_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__prioritization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tcp__tr_t4, tcp_ip_interpretation__prioritization_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(tcp__tr_t9, tcp_ip_interpretation__prioritization_reading, theater_ratio, 9, 0.2).
narrative_ontology:measurement(tcp__tr_t13, tcp_ip_interpretation__prioritization_reading, theater_ratio, 13, 0.23).
narrative_ontology:measurement(tcp__tr_t18, tcp_ip_interpretation__prioritization_reading, theater_ratio, 18, 0.26).
narrative_ontology:measurement(tcp__tr_t22, tcp_ip_interpretation__prioritization_reading, theater_ratio, 22, 0.28).
narrative_ontology:measurement(tcp__tr_t27, tcp_ip_interpretation__prioritization_reading, theater_ratio, 27, 0.3).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(tcp__be_t4, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 4, 0.37).
narrative_ontology:measurement(tcp__be_t9, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 9, 0.45).
narrative_ontology:measurement(tcp__be_t13, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 13, 0.49).
narrative_ontology:measurement(tcp__be_t18, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 18, 0.44).
narrative_ontology:measurement(tcp__be_t22, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 22, 0.52).
narrative_ontology:measurement(tcp__be_t27, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 27, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(tcp__su_t4, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(tcp__su_t9, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 9, 0.41).
narrative_ontology:measurement(tcp__su_t13, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 13, 0.46).
narrative_ontology:measurement(tcp__su_t18, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 18, 0.49).
narrative_ontology:measurement(tcp__su_t22, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 22, 0.5).
narrative_ontology:measurement(tcp__su_t27, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 27, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__prioritization_reading, resource_allocation).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial 'net neutrality debate' label conflates three structurally distinct claims about one kernel (the meaning of the TCP/IP design for traffic handling). This file is the prioritization member; neutrality_reading and zero_rating_reading are separate constraints with their own epsilon and victim structures. The upstream member (neutrality_reading, resting on the older end-to-end literature) influences the downstream members because both commercial readings must answer the non-discrimination imperative it asserts. Linked via network.affects_constraints per the epsilon-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tcp_ip_interpretation__prioritization_reading, powerful, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
