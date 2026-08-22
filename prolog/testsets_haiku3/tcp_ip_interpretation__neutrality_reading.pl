% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tcp_ip_interpretation__neutrality_reading, []).

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
 *   constraint_id: tcp_ip_interpretation__neutrality_reading
 *   human_readable: TCP/IP End-to-End Non-Discrimination Principle (Neutrality Reading)
 *   domain: technology_governance/internet_policy
 *
 * SUMMARY:
 *   This constraint instantiates the 'neutrality reading' of the contested
 *   TCP/IP kernel. The neutrality reading interprets the end-to-end principle
 *   as imposing a normative requirement: ISPs must not discriminate among
 *   packets based on their content, source application, or commercial
 *   relationships beyond standard connectivity subscriptions. This reading
 *   benefits edge innovators and content creators by protecting their access
 *   to users; it extracts regulatory cost from ISPs by constraining their
 *   revenue optimization. The reading competes with two sibling readings: the
 *   'prioritization reading' (TCP/IP permits differentiated
 *   quality-of-service as network management) and the 'zero-rating reading'
 *   (TCP/IP allows selective exemptions for sponsored content). All three are
 *   live positions held by different institutional coalitions. This story
 *   authorizes the neutrality reading's specific ε, beneficiary/victim
 *   structure, and enforcement requirements, independent of what the
 *   alternative readings would compute.
 *
 * KEY AGENTS:
 *   - Edge innovators: moderate power, mobile exit — protected by non-discrimination rule from ISP gatekeeping
 *   - ISPs/Network operators: institutional power, trapped exit — constrained from prioritizing or zero-rating content
 *   - Internet users: powerless, constrained exit — benefit from non-discriminatory access but absorb cost indirectly
 *   - Regulatory authorities: institutional power, analytical seat — author and enforce the neutrality rule
 *   - Infrastructure investors: excluded from policy table but fund litigation against neutrality constraints
 *   - Dominant platforms: observer position — publicly support neutrality but privately negotiate exceptions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, 0.68).
domain_priors:suppression_score(tcp_ip_interpretation__neutrality_reading, 0.71).
domain_priors:theater_ratio(tcp_ip_interpretation__neutrality_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__neutrality_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__neutrality_reading, "TCP/IP End-to-End Non-Discrimination Principle (Neutrality Reading)").
narrative_ontology:topic_domain(tcp_ip_interpretation__neutrality_reading, "technology_governance/internet_policy").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__neutrality_reading, '9ddcef43-4f34-478c-9b6b-1ba8e1a58339').
narrative_ontology:cs_kernel_codification('9ddcef43-4f34-478c-9b6b-1ba8e1a58339', distributed).
narrative_ontology:cs_authority_grounding('9ddcef43-4f34-478c-9b6b-1ba8e1a58339', distributed).
narrative_ontology:cs_reading_relation('9ddcef43-4f34-478c-9b6b-1ba8e1a58339', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ddcef43-4f34-478c-9b6b-1ba8e1a58339', tcp_ip_interpretation__zero_rating_reading, influences).
narrative_ontology:cs_axiom('9ddcef43-4f34-478c-9b6b-1ba8e1a58339', foundational, non_discrimination_requirement).
narrative_ontology:cs_axiom_status(non_discrimination_requirement, holdable).
narrative_ontology:cs_axiom_grounding('9ddcef43-4f34-478c-9b6b-1ba8e1a58339', non_discrimination_requirement, deontological).
narrative_ontology:cs_axiom('9ddcef43-4f34-478c-9b6b-1ba8e1a58339', foundational, edge_innovation_protection).
narrative_ontology:cs_axiom_status(edge_innovation_protection, holdable).
narrative_ontology:cs_axiom_grounding('9ddcef43-4f34-478c-9b6b-1ba8e1a58339', edge_innovation_protection, instrumental).
narrative_ontology:cs_reference_frame('9ddcef43-4f34-478c-9b6b-1ba8e1a58339', open_internet_equal_access).
narrative_ontology:cs_drift_state('9ddcef43-4f34-478c-9b6b-1ba8e1a58339', contemporary_platform_concentration_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9ddcef43-4f34-478c-9b6b-1ba8e1a58339', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, edge_innovators).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, content_creators).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, internet_users).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, internet_service_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, network_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, internet_users).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__neutrality_reading, end_to_end_principle).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__neutrality_reading, open_internet_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Startup companies and developers building applications and services at the network edge (application layer). The non-discrimination principle protects their traffic from being degraded or blocked based on content type or application identity, allowing them to compete with established players on innovation merit rather than negotiation power with ISPs. They benefit from equal network access without paying gatekeeping fees beyond standard interconnection.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, edge_innovators, beneficiary,
    moderate, biographical, mobile, global).

% Video platforms, news organizations, streaming services, and media producers. The neutrality principle prevents ISPs from throttling or blocking their content to favor competing services or to extract side payments. They operate on the assumption that their traffic reaches users at consistent quality determined by the application, not by ISP commercial relationships.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, content_creators, beneficiary,
    organized, generational, mobile, global).

% End users consuming applications and content. They benefit from non-discriminatory access: the application they choose to use reaches them at the same network quality as any other, without the ISP steering them toward preferred services. They also pay indirectly when ISP costs are absorbed into subscription prices or when they subscribe to multiple services because neutral routing makes bundling unnecessary.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, internet_users, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__neutrality_reading, internet_users, payer).

% Carriers operating the infrastructure that moves data end-to-end: broadband providers, mobile network operators. Under the neutrality reading, they are prohibited from prioritizing traffic based on its content, source, or commercial agreements beyond standard capacity pricing. They argue this constrains their ability to manage networks efficiently, monetize premium services, and incentivize infrastructure investment. They bear the cost of carrying all traffic equally while competing services capture the application-layer revenue.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, internet_service_providers, payer,
    institutional, generational, trapped, national).

% Large institutional carriers managing backbone and last-mile infrastructure globally. The neutrality constraint prevents them from operating tiered services where premium payers receive priority, from zero-rating their own applications or partners' content, or from extracting payments from content providers for fast-lane delivery. Their revenue model is constrained to connectivity subscriptions, and their competitive position against tech giants is weakened because they cannot differentiate service based on commercial partnerships.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, network_operators, payer,
    powerful, generational, trapped, global).

% National telecommunications regulators (FCC in US, BEREC in EU, national telecom commissions globally) enforcing net neutrality rules. They author and police the non-discrimination requirement, investigate violations, and adjudicate disputes between content and ISPs. Their enforcement machinery includes traffic monitoring, complaint resolution, and remedial authority over ISP conduct.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Capital providers (private equity, institutional investors) funding network buildout. They would argue that constraining ISP revenue optimization reduces the financial incentive to invest in infrastructure, particularly in lower-margin rural and underserved markets. They are excluded from the immediate decision-making about net neutrality rules but lobby regulators and fund litigation contesting the neutrality reading.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, infrastructure_investors, excluded,
    powerful, generational, arbitrage, global).

% Major technology companies (Google, Meta, Amazon, Apple, Netflix) with large-scale content and application services. They observe and influence net neutrality policy from a complex position: they are beneficiaries of non-discrimination in principle (their traffic receives equal treatment), but they have market power to negotiate favorable terms with ISPs and can absorb content delivery costs through CDNs and direct peering arrangements. They publicly support neutrality while privately seeking exceptions for their own services.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, dominant_platforms, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__neutrality_reading, internet_service_providers).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__neutrality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform rule that data packets are forwarded by ISPs based on destination address and network layer protocol, not on content, application type, or commercial relationships. This solves the collective action problem of application innovation: developers can deploy services at the network edge without needing to negotiate with every ISP globally for prioritization or access. The rule coordinates internet users' expectation that any application they install will reach them at consistent network quality.
% TRANSFER_FUNCTION: Transfers revenue opportunities from network operators to edge innovators and content providers. ISPs give up the ability to charge content providers for fast-lane access, to zero-rate partner content, to throttle competitors' traffic, and to operate tiered service tiers based on content type. This revenue loss is absorbed by ISPs as a regulatory cost; edge innovators capture the value of deployment freedom.
% ABSENT_VOICES: Infrastructure investors and rural broadband providers are structurally excluded: they argue that constraining ISP revenue optimization undercuts the financial case for building networks in marginal markets. They would advocate for differentiated pricing, managed services, and content-specific prioritization to improve ROI. Their absence from the policymaking table reflects the power asymmetry between tech advocacy coalitions and investment communities in telecommunications policy.
% DISAPPEARANCE_RATIONALE: If the neutrality constraint vanished, ISPs would immediately begin negotiating fast-lane agreements with major content providers, zero-rating would spread, and edge developers would face new gatekeeping barriers. The internet would reorganize around ISP-negotiated service tiers rather than innovation at the edge. Application discovery and distribution would shift from user choice to ISP partnership leverage. Startups without ISP relationships would face degraded service or higher user acquisition costs.
% FOUNDING_PROBLEM: Early internet governance lacked a unified rule governing ISP conduct toward traffic. This created asymmetric incentives: ISPs could extract rents from content providers through prioritization threats, while edge innovation depended on unpredictable access quality. The founding problem was framed as: 'How do we prevent ISPs from degrading the internet to gatekeep innovation and extract monopoly rents from applications?'
% FOUNDING_PROBLEM_CORROBORATION: Edge innovators and public-interest advocates attest the founding problem is live: ISPs continue to seek ways to monetize content prioritization (evident in zero-rating, sponsored data plans, and 5G tiered-service announcements). ISP industry representatives attest the founding problem is solved: modern internet quality is excellent and competition works (relying on evidence of broadband speed improvements). Telecommunications economists from outside the carrier industry attest the founding problem is partially live: congestion management does require ISP discretion, but open-internet advocates argue the founding problem is about *discriminatory* extraction, not reasonable network management.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__neutrality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__neutrality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tcp_ip_interpretation__neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__neutrality_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tcp_ip_interpretation__neutrality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tcp_ip_interpretation__neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end) because the neutrality reading imposes a real constraint on ISP conduct: it prevents them from monetizing content-based differentiation and from extracting payments from content providers. The constraint is not extraction in the piton sense (abandoned function) but rather a redistribution of revenue opportunities away from ISPs and toward edge innovators. Suppression is high (0.71) because the neutrality constraint requires active enforcement: regulators must monitor traffic, investigate complaints of discrimination, and sanction violations. ISPs have strong incentive to evade the rule (they profit from prioritization), so enforcement machinery must be robust. Theater ratio is moderate (0.42): some enforcement activity is genuine (detecting packet discrimination), but rising theater reflects the increasing sophistication of ISP workarounds (zero-rating presented as 'free services,' prioritization framed as 'network management,' congestion pricing masked as 'peak-hour rates'). The measurement series shows extractiveness and suppression both rising over the interval — the constraint's enforcement has intensified as ISPs have become more creative in their evasion tactics, requiring regulators to harden the rule and strengthen monitoring.
 *
 * PERSPECTIVAL GAP:
 *   From the edge-innovator and user seats, the neutrality reading is protective coordination: equal network access enables competition and choice. From the ISP seat, the same constraint is extractive regulation: it prevents them from monetizing their infrastructure and managing networks efficiently according to commercial demand. From the regulatory seat, the constraint is a mandate to prevent ISP market power from corrupting innovation incentives. The engine computes this divergence from the structural data: ISPs are the payers (constrained revenue, high suppression cost), innovators are the beneficiaries (protected access), and the power asymmetry is encoded in exit options (ISPs trapped, innovators mobile). The perspectival gap is NOT resolved by the author; it is the classification's job to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   ISPs carry high directionality toward the target end (d near 1.0): they bear the cost of the non-discrimination requirement through constrained revenue and high enforcement burden. The constraint actively suppresses their preferred revenue models. They have trapped exit: they cannot simply abandon the business or redefine their role. Edge innovators carry low directionality (d near 0.0): they receive protection from discrimination without running the constraint's infrastructure, and they have mobile exit (they can deploy services globally and choose where to operate). Internet users are near-symmetric: they benefit from non-discriminatory access but absorb ISP cost increases indirectly, and their exit is constrained (switching ISPs is difficult even if service quality degrades). Regulatory authorities carry analytical directionality (d = 0.5 by convention): they author the constraint but do not extract from it or bear its cost directly. Infrastructure investors are excluded (not seated in stakeholders[]): they would carry high target directionality if admitted (they bear the cost of constrained ISP revenue models), but their exclusion from decision-making is itself part of the constraint's enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The neutrality reading faces a mandatrophy challenge: the founding problem (ISPs extracting rents from content providers through prioritization threats) was acute in the 2010s when content distribution was fragmented and ISP leverage was high. The internet has evolved: dominant platforms now operate private peering networks and CDNs, ISP infrastructure quality has improved, and mobile broadband competition has increased in many markets. The founding problem is less live than it was, yet the neutrality constraint persists and its enforcement has intensified (rising suppression_requirement in measurements). This suggests the constraint may be drifting from coordination (solving a real problem) toward theater (maintaining regulatory authority regardless of whether the problem is still severe). The mandatrophy analysis does NOT conclude the constraint should be abandoned — protecting edge innovation remains important — but it flags that the rationale for enforcement intensity should be re-examined. The theater_ratio rise reflects this: regulators are spending more effort on sophisticated ISP workarounds than on genuine discrimination that threatens innovation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prioritization_vs_nondiscrimination_reading,
    'Is the end-to-end principle structurally compatible with ISP differentiation based on content type or commercial relationships, or do these practices necessarily violate the principle''s requirement that routing decisions depend only on destination address?',
    'This is a sibling-reading question, not an empirical dispute. Resolution requires committing to a reading of the technical principle and its normative implications. The prioritization_reading interprets differentiation as network management consistent with the principle; the neutrality_reading interprets it as discrimination that violates the principle. No empirical fact resolves which reading is correct — the readings coexist because they commit to different normative framings of the same technical fact.',
    'If prioritization is deemed compatible with end-to-end, regulatory authorities would shift from enforcement toward permitting tiered services, ISP revenue optimization would increase, and edge innovation incentives would weaken. If non-discrimination is deemed mandatory, enforcement intensity increases and ISP revenue remains constrained. The classification changes fundamentally: neutrality persists as a coordination mandate; prioritization becomes a matter of ISP commercial discretion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prioritization_vs_nondiscrimination_reading, conceptual, 'Kernel reading contest: is differentiation compatible with end-to-end architecture, or does it violate the principle?').

omega_variable(
    enforcement_sustainability,
    'Can regulators sustain the neutrality constraint''s enforcement burden as ISP evasion tactics become more sophisticated and as dominant platforms absorb content distribution through private networks (peering, CDNs)?',
    'Monitor the theater_ratio trajectory: if theater rises while measured discrimination stays flat, enforcement is increasingly performative rather than functional. If regulators successfully close workarounds and theater stabilizes, enforcement remains effective.',
    'If enforcement becomes unsustainable, the neutrality constraint becomes a piton: maintained by regulatory theater rather than functional necessity. The classification would shift from tangled_rope (coordination + extraction) toward piton (performance of a rule whose function has attrophied). ISPs would de facto operate tiered services even under formal neutrality rules.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_sustainability, empirical, 'Whether the neutrality constraint''s enforcement machinery can track ISP innovation in evasion tactics.').

omega_variable(
    structural_separability_of_management_and_discrimination,
    'Are network management functions (congestion handling, quality-of-service provisioning) structurally separable from content-based discrimination, or does managing network congestion inevitably require ISPs to prioritize some traffic over others based on its characteristics?',
    'Technical analysis and controlled experiments: can ISPs implement congestion management using only destination-based, application-agnostic mechanisms (time-of-use pricing, capacity auctions, destination-based routing adjustment) without inspecting packet content or application type? Or does effective management require content inspection and application-layer decisions?',
    'If separable, the neutrality reading is a clean non-discrimination requirement with room for technical network management. If inseparable, some degree of discrimination is necessary for network operation, and the neutrality reading must either permit management-based discrimination or accept network quality degradation as the cost of strict non-discrimination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_separability_of_management_and_discrimination, empirical, 'Whether congestion management requires content-based discrimination or can be achieved through application-agnostic mechanisms.').

omega_variable(
    zero_rating_compatibility,
    'Is zero-rating (exempting certain content from data caps or charges) compatible with the non-discrimination principle, or does it necessarily constitute unlawful discrimination?',
    'This is partly a reading question (how do we define discrimination?) and partly empirical (what are the competitive effects of zero-rating?). Resolution requires both a normative interpretation of ''non-discrimination'' and evidence about market effects.',
    'The zero_rating_reading interprets selective exemptions as permitted; the neutrality_reading interprets them as discrimination. If zero-rating is deemed permitted, ISPs can legally exempt their own content and partners'' content, recovering revenue and creating advantage for partnered services. If zero-rating is deemed discriminatory, ISPs face further revenue constraints and edge innovation benefits further. This directly determines whether the constraint classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(zero_rating_compatibility, conceptual, 'Kernel reading contest: is zero-rating compatible with non-discrimination, or does it necessarily violate the neutrality principle?').

omega_variable(
    suppression_mechanism_internalization,
    'Is ISP compliance with the neutrality rule maintained by structural constraints (technical architecture, regulatory monitoring, costly violation penalties) or by internalized norms (ISPs accepting non-discrimination as legitimate)?',
    'Post-compliance test: if enforcement were suspended (monitoring ceased, penalties removed), would ISPs continue non-discriminatory practices? High internalization would predict yes; low internalization would predict rapid reversion to prioritization.',
    'If suppression is structural (external monitoring, penalties), the constraint''s persistence depends on sustained regulatory effort. If internalization is high, the constraint is more stable and requires less enforcement theater. Low internalization + high regulatory fatigue predicts constraint decay toward piton classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether ISP compliance is structurally enforced or reflects internalized commitment to non-discrimination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__neutrality_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp_ip_neutrality_tr_t0, tcp_ip_interpretation__neutrality_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(tcp_ip_neutrality_tr_t0, observed).
narrative_ontology:measurement(tcp_ip_neutrality_tr_t5, tcp_ip_interpretation__neutrality_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(tcp_ip_neutrality_tr_t5, observed).
narrative_ontology:measurement(tcp_ip_neutrality_tr_t10, tcp_ip_interpretation__neutrality_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(tcp_ip_neutrality_tr_t10, observed).
narrative_ontology:measurement(tcp_ip_neutrality_tr_t15, tcp_ip_interpretation__neutrality_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(tcp_ip_neutrality_tr_t15, observed).
narrative_ontology:measurement(tcp_ip_neutrality_tr_t20, tcp_ip_interpretation__neutrality_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(tcp_ip_neutrality_tr_t20, observed).
narrative_ontology:measurement(tcp_ip_neutrality_tr_t25, tcp_ip_interpretation__neutrality_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(tcp_ip_neutrality_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(tcp_ip_neutrality_be_t0, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(tcp_ip_neutrality_be_t0, observed).
narrative_ontology:measurement(tcp_ip_neutrality_be_t5, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement_basis(tcp_ip_neutrality_be_t5, observed).
narrative_ontology:measurement(tcp_ip_neutrality_be_t10, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement_basis(tcp_ip_neutrality_be_t10, observed).
narrative_ontology:measurement(tcp_ip_neutrality_be_t15, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(tcp_ip_neutrality_be_t15, observed).
narrative_ontology:measurement(tcp_ip_neutrality_be_t20, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(tcp_ip_neutrality_be_t20, observed).
narrative_ontology:measurement(tcp_ip_neutrality_be_t25, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(tcp_ip_neutrality_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(tcp_ip_neutrality_su_t0, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(tcp_ip_neutrality_su_t0, observed).
narrative_ontology:measurement(tcp_ip_neutrality_su_t5, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(tcp_ip_neutrality_su_t5, observed).
narrative_ontology:measurement(tcp_ip_neutrality_su_t10, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(tcp_ip_neutrality_su_t10, observed).
narrative_ontology:measurement(tcp_ip_neutrality_su_t15, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(tcp_ip_neutrality_su_t15, observed).
narrative_ontology:measurement(tcp_ip_neutrality_su_t20, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(tcp_ip_neutrality_su_t20, observed).
narrative_ontology:measurement(tcp_ip_neutrality_su_t25, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(tcp_ip_neutrality_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__neutrality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__neutrality_reading, 0.12).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__prioritization_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% The neutrality_reading is one of three structurally distinct interpretations of the TCP/IP end-to-end principle kernel. The three readings compete over whether ISPs may engage in content-based differentiation. They share a common technical referent (how TCP/IP routers forward packets) but commit to different normative frameworks for what routers *should* do. Each reading has its own ε, beneficiary/victim set, and enforcement requirements. The network edges model the kernel contest: the neutrality_reading influences both sibling readings (if neutrality is enforced strictly, it constrains the conditions under which prioritization and zero-rating can occur) but does not foreclose them (all three remain live institutional positions). Readers should examine all three stories to understand the full contested landscape around net neutrality policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tcp_ip_interpretation__neutrality_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
