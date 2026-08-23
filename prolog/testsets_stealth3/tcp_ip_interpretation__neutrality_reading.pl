% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Net Neutrality Non-Discrimination Mandate (Neutrality Reading of TCP/IP)
 *   domain: technological/regulatory
 *
 * SUMMARY:
 *   The neutrality reading holds that the TCP/IP architecture embodies the
 *   end-to-end argument and therefore requires last-mile operators to carry
 *   traffic without discrimination by content, application, or source.
 *   Instantiated as regulation, the mandate prohibits ISP
 *   content/application-based discrimination, protects edge innovation, and
 *   constrains ISP revenue optimization. This file instantiates ONE reading
 *   of the tcp_ip_interpretation kernel; the prioritization and zero-rating
 *   readings are separate constraint stories linked via
 *   network.affects_constraints, each with its own epsilon over the same
 *   underlying network-operations referent. Epsilon referent: the standing
 *   arrangement under contest — last-mile broadband markets as governed by
 *   contested non-discrimination obligations — assessed by this reading's own
 *   lights: ISPs hold gatekeeper positions over captive subscribers, and
 *   wherever the mandate weakens, extraction migrates into prioritization
 *   deals, sponsored data, and interconnection leverage. The mandate's
 *   principal cost-bearer is the ISP seat; its principal defenders are the
 *   edge and subscriber seats. KEY AGENTS (by structural relationship): -
 *   broadband_isps: Primary target (institutional/constrained) — bears the
 *   mandate's costs - large_edge_platforms: Principal beneficiary
 *   (powerful/arbitrage) — reaches subscribers without carriage tolls -
 *   edge_startups_innovators: Protected beneficiary (moderate/constrained) —
 *   the permissionless-entry class the mandate shields - internet_end_users:
 *   Diffuse beneficiary and residual payer (powerless/trapped) — unsteered
 *   access, indirect price exposure - telecom_regulators: Agenda-setter
 *   (institutional/analytical) — drafts, defends, revises; collects nothing -
 *   community_broadband_operators: Excluded challenger (organized/trapped) —
 *   barred competitors shaping the consensus by absence -
 *   internet_policy_researchers: Analytical observer (analytical/analytical)
 *   — measures enforcement and investment effects
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, 0.52).
domain_priors:suppression_score(tcp_ip_interpretation__neutrality_reading, 0.62).
domain_priors:theater_ratio(tcp_ip_interpretation__neutrality_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__neutrality_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__neutrality_reading, "Net Neutrality Non-Discrimination Mandate (Neutrality Reading of TCP/IP)").
narrative_ontology:topic_domain(tcp_ip_interpretation__neutrality_reading, "technological/regulatory").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__neutrality_reading, '75899b31-5e9d-4791-b316-50f7239f2305').
narrative_ontology:cs_kernel_codification('75899b31-5e9d-4791-b316-50f7239f2305', distributed).
narrative_ontology:cs_authority_grounding('75899b31-5e9d-4791-b316-50f7239f2305', distributed).
narrative_ontology:cs_reading_relation('75899b31-5e9d-4791-b316-50f7239f2305', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_reading_relation('75899b31-5e9d-4791-b316-50f7239f2305', tcp_ip_interpretation__zero_rating_reading, coexists_with).
narrative_ontology:cs_axiom('75899b31-5e9d-4791-b316-50f7239f2305', foundational, neutral_transport_architectural_imperative).
narrative_ontology:cs_axiom_status(neutral_transport_architectural_imperative, holdable).
narrative_ontology:cs_axiom_grounding('75899b31-5e9d-4791-b316-50f7239f2305', neutral_transport_architectural_imperative, instrumental).
narrative_ontology:cs_axiom('75899b31-5e9d-4791-b316-50f7239f2305', secondary, permissionless_edge_innovation).
narrative_ontology:cs_axiom_status(permissionless_edge_innovation, holdable).
narrative_ontology:cs_axiom_grounding('75899b31-5e9d-4791-b316-50f7239f2305', permissionless_edge_innovation, instrumental).
narrative_ontology:cs_reference_frame('75899b31-5e9d-4791-b316-50f7239f2305', end_to_end_neutral_transport).
narrative_ontology:cs_drift_state('75899b31-5e9d-4791-b316-50f7239f2305', post_title_ii_repeal_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('75899b31-5e9d-4791-b316-50f7239f2305', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, large_edge_platforms).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, edge_startups_innovators).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, internet_end_users).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, broadband_isps).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, internet_end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate last-mile access networks under a prohibition on content-, application-, and source-based discrimination. They cannot sell prioritization tiers, sponsored-data exemptions, or paid-edge termination to the companies whose traffic fills their pipes, and must recover capacity costs through subscription pricing and interconnection settlements instead. Exiting the obligation would mean ceasing to operate networks, which is unavailable, so their adaptation routes into litigation, administrative-comment campaigns, and state-preemption legislation aimed at the rule's authors.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, broadband_isps, payer,
    institutional, generational, constrained, national).

% Stream, search, and commerce services that reach every subscriber over ISP last miles without per-network carriage negotiations. Under the mandate they pay transit and CDN costs but no termination tolls to access ISPs. They adapt across jurisdictions by shifting traffic among CDNs, building private backbones, and absorbing divergent national rules, so their dependence on any single regime is buffered.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, large_edge_platforms, beneficiary,
    powerful, biographical, arbitrage, global).

% Launch applications that must be reachable by every subscriber from day one, without negotiating distribution with hundreds of networks and without budget for fast-lane tolls. Uniform reachability is the precondition of their existence; their welfare is tied tightly to the mandate's survival, and they have no comparable substitute if discrimination returns.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, edge_startups_innovators, beneficiary,
    moderate, biographical, constrained, global).

% Reach lawful content and applications of their own choosing without ISP steering, blocking, or paid-tier gating. They bear costs indirectly where operators recover margin through subscription prices or deferred capacity investment. Most households face one or two viable broadband providers locally, so exit from the arrangement is not realistically available to them.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, internet_end_users, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__neutrality_reading, internet_end_users, payer).

% Draft, defend, vacate, and redraft the non-discrimination rules across administrations; litigate to establish jurisdiction; certify compliance through transparency-report review. They collect no revenue from the arrangement and alternate between readings of the underlying architecture depending on the commission's composition.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, telecom_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Municipal and cooperative network builders barred by state preemption statutes from expanding service. They would argue that competitive entry disciplines discriminatory conduct better than federal rule-making does, but they are kept out of the policy conversation by the same legislative machinery the incumbent operators influence.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, community_broadband_operators, excluded,
    organized, generational, trapped, local).

% Measure interconnection congestion, publish investment-effect studies, and map which reading of the architecture the operative rules actually instantiate. They hold no material stake in the transfer and supply the independent record the other seats cite against each other.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, internet_policy_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__neutrality_reading, diffuse).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__neutrality_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single uniformly reachable internetwork: any edge application can deliver to any subscriber without per-network carriage negotiation, and endpoint intelligence stays permissionless. It solves the fragmentation problem that per-ISP discrimination would otherwise create, in which each operator curates its own reachable set of services.
% TRANSFER_FUNCTION: Prevents a would-be transfer rather than collecting one: absent the mandate, edge providers and application sponsors would pay prioritization, termination, and sponsorship tolls to last-mile operators. The rule leaves that surplus with edge firms and subscribers, while capacity costs remain inside ISP subscription revenues.
% ABSENT_VOICES: Community broadband operators barred by state preemption statutes would argue competition, not federal rule-making, disciplines discrimination. Future application developers — the mandate's strongest intended beneficiaries — cannot yet exist to speak for themselves. Low-income subscribers bearing subscription-price and investment-deferral effects are rarely seated in technical proceedings.
% DISAPPEARANCE_RATIONALE: Overnight repeal would reopen paid-prioritization and sponsored-data markets within quarters: operators would tier delivery, large platforms would buy assurance lanes, startup reachability would become negotiable, and venture allocation would shift toward incumbency-tolerant products. The edge-innovation economy would reorganize around whatever the carriers permit.
% FOUNDING_PROBLEM: Carrier control of the last mile threatened to fragment the open network: operators could block rival applications (the 2005 VoIP-blocking consent decree), throttle disfavored traffic (the 2007 peer-to-peer episode), and tax edge innovation through termination charges — recreating the closed carriage economics of the pre-internet network.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the edge-provider beneficiary set: the FCC's 2005 enforcement action against a rural carrier for VoIP blocking, the 2008 consent decree over peer-to-peer throttling, and independent measurement-firm documentation of the 2014 interconnection congestion episode all attest gatekeeper conduct from enforcement records and neutral measurement, not from edge-firm advocacy.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__neutrality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__neutrality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tcp_ip_interpretation__neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__neutrality_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon 0.52: the mandate imposes real, rising costs on its principal regulated seat — foregone discrimination revenue grows with the streaming economy's willingness to pay for delivery assurance — while conferring diffuse benefits. It is extractive toward ISPs without being confiscatory, and this reading regards much of the constrained conduct as illegitimate double-dipping (charging both subscriber and edge for the same packets), which caps epsilon below snare range. Suppression 0.62 is raw and unscaled: the mandate survives only through active legal machinery (commission orders, jurisdictional litigation, state statutes) that closes a category of ISP business models; it suppresses conduct, not participant exit. Theater 0.32: transparency-report compliance and annual disclosure rituals grow increasingly performative relative to actual enforcement actions. Accessibility_collapse 0.35: alternatives persist on both sides — ISPs retain usage-based pricing, interconnection fees, and CDN partnerships; edge firms retain multi-CDN and private-backbone workarounds. Resistance 0.72: two decades of litigation, an outright repeal episode, and state-preemption campaigns. All three measurement series share one six-point grid (t=0..20, approximating 2005-2025); suppression_requirement is tracked because enforcement-capacity buildup and post-2017 whiplash is the dynamic this story traces. Receipt surface: gains are diffuse — each named seat was checked and none captures the mandate's operation (ISPs lose, edge seats gain avoided-cost spreads, regulators collect nothing). Fixing_cost is authored 'cheap': removal is procedurally inexpensive for the agenda-setter, demonstrated by the 2017 repeal, while durable statutory installation is the prohibitive direction — a ratchet asymmetry, not neglect.
 *
 * PERSPECTIVAL GAP:
 *   From the broadband_isps seat the mandate computes as imposed extraction: a prohibition on monetizing infrastructure they built, enforced by an agency that flips with elections. From the edge seats the identical structure computes as protective coordination: the guarantee that reachability never becomes negotiable. The regulator seat experiences it as contested jurisdiction — authority affirmed and vacated by turns. The engine derives these divergent per-seat classifications from the declared roles, power atoms, and exit options; the divergence between the payer seat and the beneficiary seats is the measurement the corpus exists to take, not an inconsistency to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   broadband_isps are declared victims and derive d near the full-target end: they bear the transfer (foregone tolls) with constrained exit — they cannot abandon their networks, so resistance routes into litigation and legislation. large_edge_platforms and edge_startups_innovators are declared beneficiaries with low d; the platforms' arbitrage-grade exit dampens their dependence further, while startups' constrained exit ties their welfare tightly to the mandate. internet_end_users carry a dual declaration (beneficiary, secondary payer): unsteered access is a direct benefit, subscription-price and investment effects are indirect costs, placing them nearer symmetric than the pure beneficiary seats. telecom_regulators administer without collecting — near-symmetric d. No directionality overrides are authored: the derivation from declared roles and exit options captures these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical errors. Reading the mandate as a snare (because a wealthy industry bears its costs) erases the genuine coordination function — a single uniformly reachable network — that explains why edge firms and users defend it. Reading it as a pure rope (because everyone shares the open internet) erases the real asymmetric cost-bearing that explains why ISPs litigate relentlessly and why enforcement machinery is load-bearing. Tangled_rope holds both truths. On mandatrophy: the founding problem — carrier gatekeeping of the last mile — remains live, corroborated by enforcement records and measurement-firm data from outside the beneficiary set; no sunset clause exists or should; mandatrophy_resolved is deliberately left undeclared. The live obsolescence risk runs the other direction: enforcement decay converting the mandate into transparency theater (tracked by the theater_ratio series and the enforcement_whiplash_persistence omega), which would push the computed type toward piton while the paper structure persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the neutrality_reading of kernel tcp_ip_interpretation; what structural deltas would the sibling readings (prioritization_reading, zero_rating_reading) introduce if adopted as the operative interpretation?',
    'Comparative classification across the three reading-stories linked in network.affects_constraints; each sibling story authors its own epsilon, beneficiaries, and victims over the same underlying network-operations referent.',
    'Under prioritization_reading, ISP differentiation becomes lawful network management — broadband_isps move from victim toward beneficiary and the extraction asymmetry inverts toward edge firms unable to pay for tiers. Under zero_rating_reading, sponsored-content exemptions reintroduce content-based asymmetry with sponsors as beneficiaries and unsponsored edge providers as victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story is one of three readings of the TCP/IP interpretation kernel; sibling readings are separate constraints.').

omega_variable(
    architectural_mandate_vs_protocol_indifference,
    'Does the TCP/IP suite genuinely entail non-discrimination (an architecture-level requirement approaching a fixed property of the design), or is the architecture formally indifferent — carrying explicit priority machinery (ToS/DSCP fields, DiffServ) — so that non-discrimination is a policy layer imposed on an ambivalent substrate?',
    'Design-history and protocol analysis: trace whether the end-to-end argument (Saltzer/Reed/Clark) functions as a derivable architectural theorem or as a design-preference argument; examine whether deployed priority mechanisms contradict or merely bracket the mandate.',
    'If the mandate is architecturally entailed, the constraint rests on a quasi-fixed foundation and resists reclassification; if the substrate is indifferent, the constraint is wholly constructed, its persistence depends entirely on enforcement politics, and drift toward piton (theatrical maintenance after enforcement decay) becomes the live risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(architectural_mandate_vs_protocol_indifference, conceptual, 'Whether non-discrimination is entailed by the architecture or layered onto an indifferent protocol suite.').

omega_variable(
    isp_investment_displacement,
    'Does the non-discrimination obligation measurably displace broadband capital investment (as broadband_isps attest), or is investment governed by subscriber demand and inter-platform competition largely independently of the rule?',
    'Difference-in-differences across jurisdictions that changed neutrality regimes (the US federal flip of 2015/2017, state laws, the EU baseline) using capex-per-subscriber series.',
    'Genuine displacement raises the real cost borne by the payer seat and strengthens the asymmetric-extraction half of the tangled_rope reading; a null effect indicates ISP cost claims are negotiation posture and the constraint sits closer to pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(isp_investment_displacement, empirical, 'Whether the mandate displaces ISP investment or ISP cost claims are rhetorical.').

omega_variable(
    interconnection_boundary_migration,
    'Does the mandate extend to interconnection and paid-peering arrangements, or only to last-mile consumer access — and has extractive pressure migrated to whichever boundary the rule leaves unregulated?',
    'Track termination-fee disputes and depeering events (including the 2014 transit congestion episode) against the rule''s textual scope; compare edge delivery costs before and after boundary rulings.',
    'If extraction migrates to the unregulated boundary, the constraint''s effective extractiveness over the whole delivery chain is lower than its last-mile profile suggests and the victim set shifts toward transit-dependent edge providers; a broad-scope reading raises ISP cost-bearing further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interconnection_boundary_migration, empirical, 'Scope boundary of the mandate and migration of extraction to unregulated interfaces.').

omega_variable(
    enforcement_whiplash_persistence,
    'Will the mandate''s enforcement survive alternating administrative reversals, or decay into transparency-report theater with the rule nominally on the books?',
    'Enforcement-action counts, complaint-resolution rates, and state-law consolidation tracked across successive regulatory cycles.',
    'Decay drives theater_ratio upward and pushes the computed type toward piton (inertial maintenance) even while the tangled_rope structure persists on paper; consolidation stabilizes the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_whiplash_persistence, empirical, 'Persistence of enforcement capacity under administrative whiplash.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__neutrality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__neutrality_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(tcp__tr_t4, tcp_ip_interpretation__neutrality_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement(tcp__tr_t8, tcp_ip_interpretation__neutrality_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(tcp__tr_t12, tcp_ip_interpretation__neutrality_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(tcp__tr_t16, tcp_ip_interpretation__neutrality_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(tcp__tr_t20, tcp_ip_interpretation__neutrality_reading, theater_ratio, 20, 0.32).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(tcp__be_t4, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(tcp__be_t8, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(tcp__be_t12, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(tcp__be_t16, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(tcp__be_t20, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(tcp__su_t4, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(tcp__su_t8, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(tcp__su_t12, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(tcp__su_t16, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(tcp__su_t20, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__neutrality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__prioritization_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% Colloquial 'TCP/IP end-to-end principle' decomposes into three structurally distinct claims per the epsilon-invariance principle: (1) this story — non-discrimination as an architectural mandate binding ISPs (epsilon 0.52, tangled_rope: genuine coordination function, real asymmetric cost-bearing, enforcement-dependent); (2) prioritization_reading — differentiated service quality as legitimate network management (relocates ISP cost-bearing from victim-side foregone revenue to service-tier income, shifting the victim set to edge firms unable to pay for tiers); (3) zero_rating_reading — selective sponsored exemptions (reintroduces content-based asymmetry with sponsors as beneficiaries). Each carries its own epsilon, stakeholders, and classification; they are linked here as one constraint family, not merged, because measuring 'what TCP/IP requires' with different observables yields different epsilon values — the label conflation, not the mathematics, is the ambiguity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
