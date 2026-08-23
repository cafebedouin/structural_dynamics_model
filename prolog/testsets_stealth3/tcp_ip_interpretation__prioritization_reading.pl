% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__prioritization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
 *   human_readable: Paid Prioritization as Network Management (Prioritization Reading of the TCP/IP Interpretation Kernel)
 *   domain: technology governance / internet policy / telecommunications law
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   tcp_ip_interpretation: the prioritization_reading, under which TCP/IP
 *   permits differentiated service quality as network management, authorizing
 *   ISPs to operate paid fast lanes, incentivizing network investment through
 *   tiered fee recovery, and disadvantaging unfunded edge services. Per the
 *   epsilon-invariance discipline, the sibling readings are different
 *   constraints in different files: the neutrality_reading authors a
 *   non-discrimination requirement over the same class of arrangement, and
 *   the zero_rating_reading authors a permission for sponsorship exemptions;
 *   they enter this file only through cs_structure.reading_relations,
 *   network.affects_constraints, and the kernel omegas. The epsilon referent
 *   is the standing arrangement under contest, assessed by this reading's own
 *   lights: the operational regime in which access networks schedule traffic
 *   into paid and unpaid classes. The reading does not deny the costs its own
 *   structural delta names, namely that unfunded edge services wait behind
 *   paying classes and that launch economics assuming budget-independent
 *   delivery stop penciling out; hence epsilon sits at a substantive
 *   middle-high level rather than near zero. Claim and metrics are authored
 *   independently: the claim is tangled_rope because the arrangement
 *   genuinely coordinates (some queue discipline is unavoidable under
 *   scarcity, and differentiated scheduling plus fee-funded capacity is a
 *   real answer) while the same structure extracts asymmetrically (those
 *   unable to pay receive systematically worse delivery), held together by
 *   active enforcement (delivery contracts, traffic-policy machinery, and
 *   continuous regulatory defense of operator discretion).
 *
 * KEY AGENTS:
 *   - broadband_isps: agenda setter and principal collector (institutional/arbitrage) — administers the prioritization regime, defines what counts as network management, collects fast-lane and interconnection premiums
 *   - large_edge_platforms: dual-positioned beneficiary-payer (powerful/mobile) — buys assured delivery, converts it into a competitive moat over smaller rivals, hedges via owned CDN and backbone footprint
 *   - small_independent_edge_services: primary bearer of costs (powerless/trapped) — cannot buy comparable assurance, waits behind paying classes, no alternative path to users
 *   - broadband_subscribers: incidental beneficiary and indirect payer (organized/constrained) — receives capacity funded by tiers, absorbs deprioritized unpaid traffic and bundle steering
 *   - cdn_and_transit_providers: secondary beneficiary (institutional/arbitrage) — sells the mitigation the arrangement's friction generates
 *   - telecom_regulators: agenda setter of the legal frame (institutional/constrained) — swings between mandating equal handling and restoring operator discretion; each swing relitigated
 *   - prospective_edge_entrants: excluded voice (powerless/trapped) — launch assumptions decided in rooms they never enter
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__prioritization_reading, 0.72).
domain_priors:suppression_score(tcp_ip_interpretation__prioritization_reading, 0.7).
domain_priors:theater_ratio(tcp_ip_interpretation__prioritization_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__prioritization_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__prioritization_reading, "Paid Prioritization as Network Management (Prioritization Reading of the TCP/IP Interpretation Kernel)").
narrative_ontology:topic_domain(tcp_ip_interpretation__prioritization_reading, "technology governance / internet policy / telecommunications law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__prioritization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__prioritization_reading, '8002f43d-c134-439c-8cc6-0a451c0eb260').
narrative_ontology:cs_kernel_codification('8002f43d-c134-439c-8cc6-0a451c0eb260', fixed_text).
narrative_ontology:cs_authority_grounding('8002f43d-c134-439c-8cc6-0a451c0eb260', lineage).
narrative_ontology:cs_interpretation_layer_present('8002f43d-c134-439c-8cc6-0a451c0eb260').
narrative_ontology:cs_reading_relation('8002f43d-c134-439c-8cc6-0a451c0eb260', tcp_ip_interpretation__neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('8002f43d-c134-439c-8cc6-0a451c0eb260', tcp_ip_interpretation__zero_rating_reading, coexists_with).
narrative_ontology:cs_axiom('8002f43d-c134-439c-8cc6-0a451c0eb260', foundational, transport_design_leaves_flow_allocation_to_operators).
narrative_ontology:cs_axiom_status(transport_design_leaves_flow_allocation_to_operators, holdable).
narrative_ontology:cs_axiom_grounding('8002f43d-c134-439c-8cc6-0a451c0eb260', transport_design_leaves_flow_allocation_to_operators, conventional).
narrative_ontology:cs_axiom('8002f43d-c134-439c-8cc6-0a451c0eb260', secondary, differentiated_scheduling_constitutes_network_management).
narrative_ontology:cs_axiom_status(differentiated_scheduling_constitutes_network_management, holdable).
narrative_ontology:cs_axiom_grounding('8002f43d-c134-439c-8cc6-0a451c0eb260', differentiated_scheduling_constitutes_network_management, conventional).
narrative_ontology:cs_reference_frame('8002f43d-c134-439c-8cc6-0a451c0eb260', allocation_indifferent_transport_abstraction).
narrative_ontology:cs_drift_state('8002f43d-c134-439c-8cc6-0a451c0eb260', contemporary_state_law_patchwork, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8002f43d-c134-439c-8cc6-0a451c0eb260', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, broadband_isps).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, large_edge_platforms).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, cdn_and_transit_providers).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, broadband_subscribers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, small_independent_edge_services).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, large_edge_platforms).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, broadband_subscribers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the last-mile access network and sets the terms under which traffic is handled: standard tiers, premium latency assurances, interconnection pricing, and acceptable-use policy. Deploys the queueing and policy machinery that sorts packets into paid and unpaid classes, collects the associated fees, and defends its discretion over what counts as network management in regulatory proceedings and court. Leaving the arrangement is not a live option; the working levers are repricing, reclassifying traffic categories, and lobbying over the legal boundary.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, broadband_isps, agenda_setter,
    institutional, generational, arbitrage, national).

% Delivers video, search, commerce, and communications at planetary scale. Purchases dedicated interconnection and premium delivery to hold performance steady at peak hours; the fee stings, but assured delivery doubles as a barrier against undercapitalized competitors. Hedges further by building cache footprints and private backbone routes into major markets, negotiating bilaterally where smaller rivals cannot get a meeting.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, large_edge_platforms, beneficiary,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, large_edge_platforms, payer).

% Runs newsletters, podcasts, indie games, community platforms, and nonprofit archives on the assumption that delivery quality does not depend on budget. At peak hours their packets wait behind paying traffic classes; buying equivalent assurance costs a far larger share of their revenue, and for many it is simply unaffordable. There is no alternative wire to the customer and no realistic path to self-provisioned distribution; the working choices are absorbing worse performance, charging users for the difference, or closing.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, small_independent_edge_services, payer,
    powerless, biographical, trapped, global).

% Buys access plans and experiences the resulting delivery quality. Gains where tiered fees fund added capacity and newer wires; loses ground where unpaid traffic queues behind sponsored or premium classes and where plan menus migrate toward bundles that privilege affiliated content. Switching providers is slow and often impossible where only one or two wires reach the home, so influence runs through complaint channels, advocacy groups, and regulators rather than exit.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, broadband_subscribers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, broadband_subscribers, payer).

% Sells caching, delivery optimization, and transit between edge networks and access networks. The rougher the last mile gets for unassured traffic, the more edge networks spend on caches and private routes, so friction at the access layer expands this market. Profits from the arrangement's side effects without setting its terms, and can reposition inventory across markets freely.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, cdn_and_transit_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Determines whether differentiated handling counts as lawful network management or unlawful discrimination, through rulemakings, adjudication, and enforcement. Authority has swung between mandating equal handling and restoring operator discretion; each definition of reasonable management redraws the boundary the other seats bargain around, and each swing draws litigation from whoever lost. Cannot exit the controversy; the docket regenerates itself.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, telecom_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Founders deciding where to build and services that have not launched. Represented in neither interconnection negotiations nor rulemaking dockets except through general-purpose advocacy; the delivery assumptions their business plans depend on, that performance does not track payments, are decided in rooms they never enter. By the time a cohort arrives, the prevailing terms are already priced in.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, prospective_edge_entrants, excluded,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__prioritization_reading, broadband_isps).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__prioritization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates genuinely scarce capacity: when access links saturate, some queue discipline must decide whose traffic waits, and differentiated scheduling resolves contention by class while the associated fees finance capacity expansion and peak-load engineering.
% TRANSFER_FUNCTION: Moves money from edge services, and indirectly from subscribers through pass-through and bundle pricing, to access-network operators as the price of latency assurance; moves reach and attention from non-paying edge services toward paying ones whenever congestion binds.
% ABSENT_VOICES: Prospective edge entrants whose launch economics assume delivery quality independent of budget; noncommercial producers, archives, and open-source infrastructure projects that can neither pay for assurance nor organize as an industry; future subscriber cohorts whose baseline expectations will form around whatever regime prevails. None of these seats sits in interconnection negotiations or rulemaking dockets except through proxy advocates.
% DISAPPEARANCE_RATIONALE: If the permission structure vanished overnight, operators would revert to uniform best-effort forwarding, premium-delivery revenue lines would close, edge delivery costs would compress toward commodity transit prices, and the interconnection market would reorganize around neutral termination; CDN spending patterns, platform architecture, and launch economics for new services would all visibly rearrange over several years.
% FOUNDING_PROBLEM: Best-effort IP offered no latency guarantee for time-sensitive traffic; differentiated queueing was engineered so real-time and mission-critical packets could jump queues during congestion, and operators later sought to recover the cost of over-provisioned networks by selling graded service tiers.
% FOUNDING_PROBLEM_CORROBORATION: Academic networking literature and the DiffServ specification record attest the engineering problem independently of operator commercial interest; independent measurement programs such as M-Lab and SamKnows document persistent peak-hour degradation from outside the industry. No external source corroborates the specifically retail fast-lane framing; consumer advocates and several competition authorities attest that the sold tiers outrun measured congestion, while operators attest the management need remains live.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__prioritization_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__prioritization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__prioritization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tcp_ip_interpretation__prioritization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__prioritization_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Base extractiveness ends at 0.72: premium-delivery pricing is decoupled from marginal forwarding cost, and the unpaid class absorbs measurable peak-hour penalty. Suppression is authored at 0.70 as a STRUCTURAL property, unscaled and distinct from the suppression_requirement series: last-mile concentration means edge services and subscribers lack alternatives regardless of which way the regulatory pendulum swings, so structural lock-in stays high and flat while ACTIVE enforcement machinery (the series) oscillates. Theater ratio ends at 0.48: the network-management label legitimately covers real congestion scheduling, but a growing share of activity is retail upsell marketed through the same language, plus the pronounced compliance-performance phase during the regulated interlude. Accessibility collapse 0.60: mitigation (CDNs, direct peering) exists and works, but is purchasable mainly by the already-large, so alternatives collapse for the weak and persist for the strong. Resistance 0.70: a two-decade advocacy and litigation campaign, state-level statutes, and recurring federal rulemaking keep the arrangement continuously defended rather than settled. Temporal design uses ONE shared grid at nine points mapped approximately to 1996-2026 (unit roughly one year; anchors: DiffServ-era research at 0, first discriminatory-blocking enforcement near 8, appellate invalidation of early rules and the interconnection disputes near 16, the Title II reclassification interlude at 18, repeal near 21, the state-law and EU-regulation patchwork near 25). The cycle is a regulatory pendulum: extractiveness and enforcement machinery climb in permissive windows, collapse during the mandated-equality interlude, then rebuild. Critically, the oscillation functions partly as intermittent reinforcement with an upward RATCHET: each permissive window banks contracts, traffic-policy infrastructure, and normalized premium products, so successive troughs rise (0.35, 0.48) and successive peaks rise (0.66, 0.72). The theater spike at the interlude reflects compliance performance rather than functional change, which is why theater and extractiveness move inversely there.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. From the operator seat the arrangement is coordination it built and staffs: congestion is real, somebody must allocate, and the fees fund wires. From the small edge seat the identical structure is a toll booth it cannot afford with no road around it. The large-platform seat experiences a manageable fee attached to a moat; the regulator seat experiences a movable legal boundary; the CDN seat experiences demand growth. The engine derives these divergent per-seat classifications from the structural data (roles, power, exit, scope); nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation: broadband_isps sit at the beneficiary pole (they author and collect); small_independent_edge_services sit at the full-target end (declared victim, powerless, trapped, so amplification is maximal); broadband_subscribers land near symmetric (genuine capacity benefit against indirect fee incidence and bundle steering); cdn_and_transit_providers sit beneficiary-side without administering anything. One override is authored: large_edge_platforms derive near the beneficiary pole from their beneficiary role, but they are also material PURCHASERS of the fast-lane product, a fee-bearing position the derivation cannot see inside a beneficiary declaration. Their net relationship mixes contribution and capture, so an override at power_atom=powerful (the only powerful seat in this story, uniquely identifying them) sets d=0.35, moving that agent off the pure-beneficiary end toward moderately-target-side-of-symmetric. telecom_regulators carry an agenda-setting role without collecting the arrangement's gains; their directionality reflects administration of the legal frame rather than rent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem splits cleanly: the engineering problem (latency-sensitive traffic over contended links) remains live and externally corroborated by queuing theory and measurement studies; the commercial framing (retail fast lanes priced to consumers of assurance) is contested, with external sources arguing the sold tiers outrun measured congestion. Status is therefore authored 'contested', and paired with disappearance_verdict=world_rearranges this yields NO dead-mandate mismatch flag; the arrangement's persistence tracks a still-live underlying need, which is exactly what keeps this a tangled_rope rather than a decayed residue. The classification guards against mislabeling in BOTH directions: a pure-coordination reading (the operator's own account) erases who pays and how asymmetrically; a pure-extraction reading (the strongest neutrality account) denies the real allocation function that scarcity forces on any shared link. The forward risk to watch is piton-drift: if the congestion_authenticity omega resolves toward scarcity-independent activation and the capex_incidence omega resolves toward substitution, the arrangement's function atrophies into a billing category maintained by brand theater, and the theater trajectory (already elevated at 0.48 with a compliance-performance spike in the series) is the leading indicator.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file is one reading of the tcp_ip_interpretation kernel; what exactly do the sibling files assert differently, and how must cross-file comparison be indexed?',
    'Join all three stories on kernel_id: tcp_ip_interpretation__neutrality_reading authors epsilon over the same differentiated-handling arrangement judged as forbidden discrimination; tcp_ip_interpretation__zero_rating_reading authors epsilon over sponsorship exemptions. Compare per-seat outputs ACROSS the three files; never treat any single file''s verdict as a fact about the kernel itself.',
    'Comparison that ignores reading index merges three distinct epsilon values and three distinct victim sets into one spurious number; per-seat divergence inside this file is valid only relative to this reading''s authorization premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-index discipline: this story is the prioritization_reading instantiation, not the kernel.').

omega_variable(
    nondiscrimination_premise_location,
    'Does the transport layer''s design constitute a nondiscrimination commitment, or is it an allocation-indifferent abstraction that leaves flow-handling policy to operators?',
    'Architectural-historical analysis of the design record (RFC 791/793, the end-to-end arguments paper, contemporaneous design-philosophy writing) cross-checked against how adjudicating bodies have treated the design''s normative content; if the record shows constitutive nondiscrimination, this reading''s authorization premise collapses into bare policy preference.',
    'Resolved as constitutive, this constraint''s permission rests entirely on constructed law and its extraction is fully attributable to policy choice; resolved as abstraction, the sibling neutrality reading''s mandate premise lacks design grounding and its constraint is equally constructed. Either resolution relocates, but does not dissolve, the inter-reading dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nondiscrimination_premise_location, conceptual, 'Where the readings'' disagreement is located: constitutive nondiscrimination versus allocation-open transport.').

omega_variable(
    congestion_authenticity,
    'How much of deployed differentiated handling responds to measured congestion, and how much proceeds independent of link saturation?',
    'Telemetry audits correlating quality-of-service activation windows with utilization curves; disclosure of what triggers premium classes in operator policy engines.',
    'If prioritization routinely activates absent congestion, the coordination component shrinks and the arrangement drifts toward pure toll collection; if tied to genuine peaks, a larger share of measured extraction is the irreducible price of the allocation function itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congestion_authenticity, empirical, 'Whether the engineering warrant matches deployment practice.').

omega_variable(
    capex_incidence_vs_rent,
    'Does fast-lane and premium-tier revenue fund incremental capacity, or substitute for capital expenditure operators would have undertaken anyway?',
    'Segmented financial disclosure linking delivery-product revenue to capital-expenditure deltas; natural experiments across jurisdictions with different permission regimes.',
    'If substitutional, the reading''s own investment-defense fails on instrumental grounds and the transfer stands nearly bare; if additive, part of the transfer is genuine funding of the coordination function and the extraction estimate should be discounted accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capex_incidence_vs_rent, empirical, 'Tests the reading''s economic warrant from within its own lights.').

omega_variable(
    small_edge_coalition_power,
    'Can individually powerless small edge providers convert collective organization, such as trade associations, cooperative peering, or pooled delivery purchasing, into bargaining power that changes their effective position?',
    'Track formation and outcomes of edge-provider coalitions in interconnection and rulemaking proceedings; measure whether pooled arrangements obtain terms comparable to large-player bilateral deals.',
    'Successful coalition shifts this seat off the pure-target corner and softens the asymmetry driving the extraction reading; repeated failure confirms the structural trap and supports the current directionality assignment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(small_edge_coalition_power, empirical, 'Coalition potential of a fragmented payer class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__prioritization_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__prioritization_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(tcp__tr_t4, tcp_ip_interpretation__prioritization_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(tcp__tr_t8, tcp_ip_interpretation__prioritization_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(tcp__tr_t12, tcp_ip_interpretation__prioritization_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(tcp__tr_t16, tcp_ip_interpretation__prioritization_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(tcp__tr_t18, tcp_ip_interpretation__prioritization_reading, theater_ratio, 18, 0.55).
narrative_ontology:measurement(tcp__tr_t21, tcp_ip_interpretation__prioritization_reading, theater_ratio, 21, 0.46).
narrative_ontology:measurement(tcp__tr_t25, tcp_ip_interpretation__prioritization_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement(tcp__tr_t30, tcp_ip_interpretation__prioritization_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tcp__be_t4, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(tcp__be_t8, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(tcp__be_t12, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(tcp__be_t16, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(tcp__be_t18, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 18, 0.48).
narrative_ontology:measurement(tcp__be_t21, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 21, 0.6).
narrative_ontology:measurement(tcp__be_t25, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(tcp__be_t30, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(tcp__su_t4, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(tcp__su_t8, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(tcp__su_t12, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(tcp__su_t16, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(tcp__su_t18, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 18, 0.24).
narrative_ontology:measurement(tcp__su_t21, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 21, 0.44).
narrative_ontology:measurement(tcp__su_t25, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 25, 0.5).
narrative_ontology:measurement(tcp__su_t30, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__prioritization_reading, resource_allocation).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the net neutrality debate': one kernel (tcp_ip_interpretation), three readings, three structurally distinct constraints with distinct epsilon values and victim sets. This file authors the prioritization_reading. The neutrality_reading file supplies the doctrinal baseline this reading defines itself against (upstream in the argumentative sense); the zero_rating_reading file rides the permissive momentum this reading normalizes, since acceptance of performance-tier differentiation lowers the political cost of accepting sponsorship exemptions. Links are declared reciprocally through network.affects_constraints in all three files; cross-family analysis must join on kernel_id, never average across files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tcp_ip_interpretation__prioritization_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
