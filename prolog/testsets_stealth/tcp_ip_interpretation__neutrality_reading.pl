% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: TCP/IP End-to-End Non-Discrimination Requirement (Neutrality Reading)
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   This file instantiates the neutrality_reading of the
 *   tcp_ip_interpretation kernel: the claim that the internet's end-to-end
 *   design logic requires operators to treat packets without regard to
 *   content, application, or sponsor — ISPs prohibited from
 *   content/application-based discrimination, edge innovation protected as a
 *   structural default, ISP revenue optimization confined to the access
 *   relationship. The arrangement the story is about, and therefore the ε
 *   referent under the kernel-reading rule, is the standing broadband
 *   traffic-governance regime: last-mile operators exercising — and actively
 *   defending, in litigation and legislation — discretion over how traffic is
 *   treated, under non-discrimination rules that have been adopted (2010,
 *   2015), vacated (2014), repealed (2017), restored (2024), and vacated
 *   again (2025) across the interval. The neutrality reading assesses that
 *   standing arrangement by its own lights and finds it substantially
 *   extractive from the edge: prioritization, zero-rating, and
 *   interconnection-toll value concentrate at the operator seat while edge
 *   developers, users, and public-interest services bear the costs. The
 *   reading's endorsed alternative — a neutrality-compliant regime — is NOT
 *   the referent and is not measured here. Sibling readings
 *   (prioritization_reading, zero_rating_reading) are separate constraints in
 *   the same kernel family, linked via network.affects_constraints; they are
 *   not averaged into this file. Claim and metrics are authored
 *   independently: claimed_type tangled_rope is this authoring seat's
 *   structural judgment (a genuine coordination function plus asymmetric
 *   extraction, held by active enforcement); the metrics describe the
 *   standing arrangement's observed operation.
 *
 * KEY AGENTS:
 *   - large_last_mile_isps: primary agenda-setter and receipt seat (institutional/arbitrage) — administers traffic treatment, sets interconnection terms, collects the arrangement's fees, and funds the defense of its legal boundary
 *   - vertically_integrated_isp_services: concentrated secondary beneficiary (powerful/mobile) — gains performance and price advantage from affiliated treatment of the transport side
 *   - edge_application_developers: primary payer (moderate/constrained) — bears the toll on reaching their own customers through ISP termination
 *   - residential_broadband_users: payer (powerless/constrained) — bears degraded access and passed-through costs; individually weak but with documented coalition capacity
 *   - public_interest_edge_services: payer (powerless/trapped) — no revenue model to pay quality fees and no substitute channel to their audiences
 *   - incumbent_large_edge_platforms: payer with arbitrage-grade exit (powerful/arbitrage) — same nominal edge class as small developers but able to absorb or route around tolls
 *   - municipal_broadband_initiatives: excluded seat (moderate/trapped) — would offer non-discriminatory access; barred from the market and the conversation by preemption statutes
 *   - communications_regulators: observer/enforcement seat (institutional/analytical) — authority vacated and rebuilt repeatedly across the interval
 *   - courts_and_legislatures: agenda-setter of the legal boundary (institutional/analytical) — the arrangement holds only within the line they draw
 *   - standards_engineering_community: analytical observer (organized/analytical) — holds the end-to-end design tradition and attests the founding problem from outside the beneficiary set, with no enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, 0.72).
domain_priors:suppression_score(tcp_ip_interpretation__neutrality_reading, 0.64).
domain_priors:theater_ratio(tcp_ip_interpretation__neutrality_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__neutrality_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__neutrality_reading, "TCP/IP End-to-End Non-Discrimination Requirement (Neutrality Reading)").
narrative_ontology:topic_domain(tcp_ip_interpretation__neutrality_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__neutrality_reading, '4d637e7e-79c8-42f6-8268-51c99060f668').
narrative_ontology:cs_kernel_codification('4d637e7e-79c8-42f6-8268-51c99060f668', distributed).
narrative_ontology:cs_authority_grounding('4d637e7e-79c8-42f6-8268-51c99060f668', distributed).
narrative_ontology:cs_reading_relation('4d637e7e-79c8-42f6-8268-51c99060f668', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d637e7e-79c8-42f6-8268-51c99060f668', tcp_ip_interpretation__zero_rating_reading, forecloses).
narrative_ontology:cs_axiom('4d637e7e-79c8-42f6-8268-51c99060f668', foundational, packet_treatment_must_be_content_blind).
narrative_ontology:cs_axiom_status(packet_treatment_must_be_content_blind, holdable).
narrative_ontology:cs_axiom_grounding('4d637e7e-79c8-42f6-8268-51c99060f668', packet_treatment_must_be_content_blind, instrumental).
narrative_ontology:cs_axiom('4d637e7e-79c8-42f6-8268-51c99060f668', secondary, application_selection_resides_at_the_edges).
narrative_ontology:cs_axiom_status(application_selection_resides_at_the_edges, holdable).
narrative_ontology:cs_axiom_grounding('4d637e7e-79c8-42f6-8268-51c99060f668', application_selection_resides_at_the_edges, instrumental).
narrative_ontology:cs_reference_frame('4d637e7e-79c8-42f6-8268-51c99060f668', content_blind_best_effort_commons).
narrative_ontology:cs_drift_state('4d637e7e-79c8-42f6-8268-51c99060f668', contemporary_broadband_regime, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4d637e7e-79c8-42f6-8268-51c99060f668', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, large_last_mile_isps).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, vertically_integrated_isp_services).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, edge_application_developers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, residential_broadband_users).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, public_interest_edge_services).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, incumbent_large_edge_platforms).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__neutrality_reading, end_to_end_principle).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__neutrality_reading, common_carriage_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the last-mile access networks over which residential traffic passes, set interconnection terms with edge networks, and decide — within whatever legal boundary is in force — how traffic is treated: what is blocked, throttled, prioritized, or exempted from metering. Collect subscription revenue from users and, where permitted, fees from edge services for quality or exemption. When non-discrimination rules are in force they fund litigation and lobbying to move the legal boundary; when rules lapse they introduce prioritization and zero-rating products. Exiting access markets is not realistic, but regulatory arbitrage is: investment and product design shift toward the least-constrained jurisdiction and segment.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, large_last_mile_isps, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__neutrality_reading, large_last_mile_isps, beneficiary).

% Content and application services owned by the same corporate groups that own the pipes — video streaming, cloud, and advertising arms. When the transport side exempts affiliated services from metering or routes them over better-performing paths, these services gain a performance and price advantage over unaffiliated rivals. They can be divested or shut down when corporate strategy changes, so their stake in the arrangement is held loosely.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, vertically_integrated_isp_services, beneficiary,
    powerful, biographical, mobile, national).

% Build the applications and services that run over the access networks and reach users only through ISP termination. When operators charge for interconnection quality, prioritize paying traffic, or exempt sponsors from data caps, these developers face a toll on reaching their own customers; they can absorb it, pass it to users, or scale back. Their alternatives — different ISPs, self-provisioned last-mile — are largely unavailable at their scale.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, edge_application_developers, payer,
    moderate, biographical, constrained, global).

% Pay subscription fees for access and experience the arrangement as the performance of the applications they choose: a throttled video service, a zero-rated rival, a capped stream. Most households can choose between one or two providers and switching is costly; their influence on the arrangement's terms arrives mainly through comment dockets, elections, and advocacy organizations rather than through market exit.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, residential_broadband_users, payer,
    powerless, biographical, constrained, national).

% Libraries, schools, telehealth providers, and civic or nonprofit services delivering over the same access networks with no revenue model from which to pay quality fees. If operators may sell performance or exemptions, these services compete for attention against sponsored rivals without the means to buy placement, and they have no substitute channel to their audiences.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, public_interest_edge_services, payer,
    powerless, generational, trapped, national).

% Large streaming, search, and social platforms whose traffic dominates access networks. They are the counterparties in interconnection and paid-prioritization disputes and have paid settlements to end them; they also operate their own content-delivery infrastructure and cache inside ISP networks, so they can route around some tolls and absorb others. Scale lets them treat quality fees as a cost of doing business — a cost their smaller competitors cannot symmetrically bear.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, incumbent_large_edge_platforms, payer,
    powerful, biographical, arbitrage, global).

% City- and community-owned network projects that would offer access on non-discriminatory terms as a condition of their charters. In many states they are barred from expanding or from offering service at all by preemption statutes promoted by incumbent operators; they sit outside the arrangement's rule-making conversations and reach it only through courts and ballot measures.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, municipal_broadband_initiatives, excluded,
    moderate, generational, trapped, local).

% Agencies — the FCC in the United States, BEREC and national regulators in Europe — that define and enforce the legal boundary of traffic treatment: adopting open-internet rules, adjudicating complaints, requiring disclosure. Their authority has been vacated, rebuilt, and vacated again across the interval, and each rebuild is immediately litigated by the operators they would constrain.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, communications_regulators, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__neutrality_reading, communications_regulators, agenda_setter).

% Appellate courts and national legislatures that set the outer boundary: striking down or upholding regulatory authority (Comcast v. FCC, Verizon v. FCC, the Sixth Circuit's 2025 vacatur of the 2024 restoration), repealing or restoring rules by statute or order, and preempting or permitting municipal networks. They do not administer traffic treatment, but nothing in the arrangement holds without the boundary they draw.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, courts_and_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% The IETF, the W3C, and the academic network-engineering tradition that designed and maintains the protocol suite and articulated the end-to-end argument. They hold normative authority over what the architecture's design logic is, publish and teach it, and have consistently described the network's job as content-blind packet delivery — but they command no enforcement power over commercial operators.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, standards_engineering_community, observer,
    organized, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__neutrality_reading, large_last_mile_isps).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__neutrality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The standing arrangement coordinates shared use of access-network capacity: ISPs finance, build, and operate last-mile infrastructure, allocate bandwidth among millions of users and edge services, and manage congestion and security. Stated without evaluation, it solves the real problem of operating and financing access networks and of allocating shared capacity among competing uses.
% TRANSFER_FUNCTION: Moves payment in both directions: users pay operators for access subscriptions; edge services increasingly pay (or are pressed to pay) for interconnection quality, prioritization, or exemption from metering; and it transfers discretion over application-level performance from the edge — where the architecture's design logic places it — to the network operator.
% ABSENT_VOICES: Municipal and cooperative network operators (barred by state preemption in much of the United States), unaffiliated small edge developers without trade-association representation, and end users as a diffuse class — none sits inside the legislative negotiation rooms or technical standard-setting conversations where the arrangement's boundary is set; their objection arrives only through comment dockets, ballot measures, and litigation by proxy organizations.
% DISAPPEARANCE_RATIONALE: If the gatekeeping arrangement vanished overnight — if last-mile operators lost discretion to treat traffic by content, application, or sponsor — paid-prioritization and zero-rating revenue lines would close, interconnection disputes would re-price around raw capacity, edge services would reach users on uniform terms, and operator business models would shift toward pure access subscription; the edge innovation pipeline would reorganize around the restored default within product cycles.
% FOUNDING_PROBLEM: Early broadband buildout was financed on the premise that network operators control their networks end to end: the arrangement consolidated traffic treatment as private operational discretion, solving the operator-side problem of recouping access-network capital costs and managing capacity as the operator judges necessary.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the standards and engineering community attests the founding design problem was content-blind packet delivery, not application governance; regulatory-record economics (the FCC's 2015 Title II findings, BEREC assessments) attest that access-network capital is substantially recouped through subscription revenue, supporting the shifted-function reading; ISP trade associations (USTA, CTIA filings) attest the financing and network-management problem remains live. The engineering-community attestation is the cleanest source outside the beneficiary set; no single outside attestation is dispositive.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__neutrality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__neutrality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tcp_ip_interpretation__neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__neutrality_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.72: the contested layer of the standing arrangement — paid prioritization, sponsored zero-rating, interconnection tolls — concentrates receipt at the operator seat while the costs fall on actors with constrained or no exit; the access-subscription layer is treated as the arrangement's legitimate coordination core, not as extraction. Suppression 0.64 is the raw structural value (unscaled by scope or power in the engine's arithmetic): last-mile concentration (one to two providers for most US households), state preemption of municipal networks, and two decades of litigation against regulatory authority. Theater 0.52: network-management justifications do genuine engineering work (congestion and security management are real) but a substantial share is cover — the FCC's 2008 Comcast finding that BitTorrent blocking was application-targeted rather than congestion-driven, and the 'free data' framing of sponsored zero-rating, are the paradigm cases; the ratio rose through the repeal-era investment narratives that operators' own filings contradicted. Accessibility_collapse 0.60: alternatives partly collapse once the arrangement is understood — self-provisioned last-mile is prohibitive, municipal entry is statutorily barred in much of the market, fixed-wireless is not yet a full substitute — but do not collapse completely. Resistance 0.62: sustained across the interval from edge companies, public-interest litigators, state legislatures (California SB-822), and user coalitions. The measurement series run on one shared grid (2005, 2009, 2013, 2015, 2017, 2021, 2024, 2025) with all three metrics authored at every point. The series are CYCLICAL, not monotonic: extraction is damped when rules are in force (2015, 2024), rises in vacatur windows (2013-2014, 2021, 2025), and suppression_requirement peaks when the enforcement machinery and the counter-mobilization against it are both at maximum (2015-2017). The oscillation is itself partly the mechanism — intermittent enforcement lets discriminatory practices accumulate in each vacatur window and raises the cost of the next reform, an intermittent-reinforcement structure rather than noise. base_properties were measured at the interval end (2025), an ungoverned phase of the cycle, which is why the scalars sit at the high edge of the series.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is structural, not rhetorical. From the operator seat the arrangement is the legitimate exercise of control over networks it financed — coordination it provides and defends. From the small-developer and public-interest seats the same structure operates as a toll gate on reach, with no alternative channel. From the large-platform seat it is a negotiable cost: the same nominal edge class as small developers, differentiated by arbitrage-grade exit (own CDNs, in-network caching, settlement capacity) — the same-level lateral contrast this scenario exists to capture. From the regulator's seat it is a boundary to be drawn and redrawn; from the courts' seat, a question of agency authority; from the engineering community's seat, a departure from the design logic it stewards. Inter-institutionally, the regulator and the operator hold the same formal power atom but opposite directionalities, and the regulator's enforcement capacity has itself been the contested object — which is why suppression_requirement, not just extraction, is the series worth tracking.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to real receipt: operators and their affiliated services collect the prioritization, exemption, and toll value, so both seats derive low d (near the beneficiary end). The victim declarations map to the bearing side: small developers (constrained exit keeps d high), users (constrained exit; their victimhood is degraded termination, not their subscription payment), and public-interest services (trapped, highest d). The large-platform seat is the deliberate ambiguity: as a payer it derives toward the target end, but its arbitrage-grade exit damps d toward the middle — the derivation's damping is correct as far as it goes, and the residual question (do the tolls stop at the platforms or pass through to users?) is carried by the edge_toll_incidence omega rather than by an override. No directionality_overrides are authored: the available override surface is power-atom-grained, and every power atom in this story holds seats with genuinely different structural positions (the 'powerful' atom alone spans the operators' content arms, the large platforms, and would misassign if overridden), so per-atom overrides would do more damage than the derivation's coarseness.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification guards against two mislabelings. Reading the arrangement as pure coordination — the operator's own network-management frame — would erase the concentrated receipt seat and the suppressed alternatives; the gate forces the story to name who is coordinated and who pays through the same structure. Reading it as pure extraction would erase the genuine financing-and-operation function (access networks are real infrastructure with real costs) and would predict abolition rather than boundary-drawing as the remedy. On the R5 genealogy: the founding problem (recouping buildout capital, operational control) is contested rather than dead — subscription revenue substantially recoups capital per the regulatory record, but operators genuinely still manage capacity — so this is not a zombie arrangement; what is contested is its SCOPE (application-level discretion), not its existence. Mandatrophy resolution therefore turns on the boundary question carried by the architecture_normativity_status omega: if the end-to-end logic is a normative architectural requirement, the application-discretion layer is a mandate that outlived its justification and should be stripped; if it is one design philosophy among several, the arrangement is an ongoing policy contest with no atrophied mandate to resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This constraint is one reading (neutrality_reading) of the tcp_ip_interpretation kernel — what would the sibling readings change structurally if instantiated instead?',
    'Authoring the sibling files: prioritization_reading would recast operator traffic treatment as coordination provision rather than receipt, lower ε over the same referent, and narrow the victim set to degraded non-paying traffic; zero_rating_reading would narrow the victim set to unsponsored services and reclassify sponsored exemptions as coordination.',
    'This file''s classification is stable under its own reading; the corpus-level verdict on the kernel depends on which reading the evidence supports. Cross-reading comparison is valid only because all three share the referent with reading-indexed values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Committer structure: kernel membership, reading identity, and sibling structural deltas.').

omega_variable(
    architecture_normativity_status,
    'Is the end-to-end principle a normative requirement built into the architecture (which the standing arrangement violates) or a design philosophy operators may rationally depart from?',
    'Protocol analysis and engineering-history work on whether application-level discrimination is technically necessary for the architecture''s functions; the IETF tradition''s own statements; evidence on whether neutrality-compliant operation degrades network performance.',
    'If a normative requirement, the standing arrangement violates a structural feature of the network and the neutrality reading approaches an architectural fact; if a philosophy, the reading is one governance preference among several and the arrangement is a contested policy choice rather than a violation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(architecture_normativity_status, conceptual, 'Whether the kernel''s design logic binds operators normatively or descriptively — the location of the sibling disagreement.').

omega_variable(
    last_mile_concentration_extent,
    'How concentrated is last-mile access in the markets the arrangement governs — is operator traffic treatment disciplined by effective competition or backed by termination monopoly?',
    'Market-concentration data, household choice counts, switching rates, and fixed-wireless and municipal substitution rates by market.',
    'Where competition is effective, gatekeeping is disciplined and ε falls toward coordination cost; where termination monopoly holds, ε rises and the victim seats'' constrained exit hardens toward trapped.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(last_mile_concentration_extent, empirical, 'The market-structure fact the extraction estimate most depends on.').

omega_variable(
    enforcement_oscillation_persistence,
    'Will the vacatur–reform oscillation in non-discrimination enforcement continue, or stabilize in either a durable-rules or a durable-deregulation state?',
    'Appellate outcomes on successors to the 2024 restoration; statutory action; the trajectory of agency-authority jurisprudence (major-questions doctrine as applied to communications regulation).',
    'Durable rules would damp the standing arrangement''s extraction toward the coordination floor; durable deregulation would let extraction ratchet as practices normalize; continued oscillation sustains the intermittent-reinforcement dynamic in which each vacatur window lets practices accumulate that survive the next reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_oscillation_persistence, empirical, 'Whether the cyclical measurement pattern resolves or persists.').

omega_variable(
    suppression_structural_split,
    'Is the suppression of alternatives — single-provider markets, barred municipal networks — structural (last-mile economics) or constructed (state preemption statutes, regulatory defunding)?',
    'Comparative analysis of markets with and without preemption statutes; entry-cost curves for competitive last-mile buildout; observed entry where preemption is repealed or waived.',
    'If mostly constructed, the arrangement''s suppression is policy-contingent and could collapse quickly with statutory change, lowering measured suppression; if structural, suppression persists regardless of statute and the arrangement''s hold is deeper than its legal defense suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_split, empirical, 'Structural versus constructed composition of the suppression metric.').

omega_variable(
    edge_toll_incidence,
    'Who ultimately bears the edge-side tolls (paid prioritization, interconnection settlements, sponsored-exemption economics) — the platforms that nominally pay, or end users through passed-through prices and degraded unsponsored service?',
    'Price and quality pass-through studies around known settlement events (the 2014 interconnection disputes are the cleanest natural experiment); platform margin analysis over the settlement periods.',
    'If incidence lands on users, the victim set widens to the whole user base and the arrangement''s reach is broader than the payer list shows; if platforms absorb it, the victim set is narrower and the large-platform seat''s mid-range derived directionality is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(edge_toll_incidence, empirical, 'Incidence of the edge-side tolls — determines the true victim-set boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__neutrality_reading, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t2005, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement_basis(tcp__tr_t2005, observed).
narrative_ontology:measurement(tcp__tr_t2009, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2009, 0.38).
narrative_ontology:measurement_basis(tcp__tr_t2009, observed).
narrative_ontology:measurement(tcp__tr_t2013, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2013, 0.45).
narrative_ontology:measurement_basis(tcp__tr_t2013, observed).
narrative_ontology:measurement(tcp__tr_t2015, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2015, 0.5).
narrative_ontology:measurement_basis(tcp__tr_t2015, observed).
narrative_ontology:measurement(tcp__tr_t2017, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2017, 0.52).
narrative_ontology:measurement_basis(tcp__tr_t2017, observed).
narrative_ontology:measurement(tcp__tr_t2021, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2021, 0.48).
narrative_ontology:measurement_basis(tcp__tr_t2021, observed).
narrative_ontology:measurement(tcp__tr_t2024, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2024, 0.5).
narrative_ontology:measurement_basis(tcp__tr_t2024, observed).
narrative_ontology:measurement(tcp__tr_t2025, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2025, 0.52).
narrative_ontology:measurement_basis(tcp__tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(tcp__be_t2005, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement_basis(tcp__be_t2005, observed).
narrative_ontology:measurement(tcp__be_t2009, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2009, 0.58).
narrative_ontology:measurement_basis(tcp__be_t2009, observed).
narrative_ontology:measurement(tcp__be_t2013, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2013, 0.66).
narrative_ontology:measurement_basis(tcp__be_t2013, observed).
narrative_ontology:measurement(tcp__be_t2015, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement_basis(tcp__be_t2015, observed).
narrative_ontology:measurement(tcp__be_t2017, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2017, 0.6).
narrative_ontology:measurement_basis(tcp__be_t2017, observed).
narrative_ontology:measurement(tcp__be_t2021, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2021, 0.7).
narrative_ontology:measurement_basis(tcp__be_t2021, observed).
narrative_ontology:measurement(tcp__be_t2024, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2024, 0.66).
narrative_ontology:measurement_basis(tcp__be_t2024, observed).
narrative_ontology:measurement(tcp__be_t2025, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2025, 0.72).
narrative_ontology:measurement_basis(tcp__be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t2005, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2005, 0.35).
narrative_ontology:measurement_basis(tcp__su_t2005, observed).
narrative_ontology:measurement(tcp__su_t2009, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2009, 0.45).
narrative_ontology:measurement_basis(tcp__su_t2009, observed).
narrative_ontology:measurement(tcp__su_t2013, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2013, 0.6).
narrative_ontology:measurement_basis(tcp__su_t2013, observed).
narrative_ontology:measurement(tcp__su_t2015, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement_basis(tcp__su_t2015, observed).
narrative_ontology:measurement(tcp__su_t2017, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2017, 0.72).
narrative_ontology:measurement_basis(tcp__su_t2017, observed).
narrative_ontology:measurement(tcp__su_t2021, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2021, 0.58).
narrative_ontology:measurement_basis(tcp__su_t2021, observed).
narrative_ontology:measurement(tcp__su_t2024, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2024, 0.66).
narrative_ontology:measurement_basis(tcp__su_t2024, observed).
narrative_ontology:measurement(tcp__su_t2025, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2025, 0.64).
narrative_ontology:measurement_basis(tcp__su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__neutrality_reading, resource_allocation).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__prioritization_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'network neutrality' — and the kernel 'what the TCP/IP architecture requires' — covers three structurally distinct claims, decomposed per the ε-invariance principle into one constraint family: that the architecture requires non-discrimination (this file), that it permits managed differentiation as network management (prioritization_reading), and that it permits sponsored exemptions (zero_rating_reading). Each reading authors its own ε over the same standing arrangement (shared referent, reading-indexed values) with its own beneficiary/victim structure. This file instantiates the neutrality reading. Within the family, the neutrality reading structurally forecloses the zero-rating reading (sponsored exemption is content/application-based treatment, which the neutrality premise prohibits — no single framework holds both core premises) while coexisting with the prioritization reading as a rival interpretation of the same design logic, bounded by where each draws the network-management line.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
