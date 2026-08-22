% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__zero_rating_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
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
 *   human_readable: Sponsored Data-Cap Exemption Arrangement (Zero-Rating Reading of TCP/IP)
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   The colloquial label 'what TCP/IP allows' decomposes into three
 *   structurally distinct claims about the same protocol kernel, each with
 *   its own epsilon and its own victim set. This story instantiates the
 *   zero-rating reading: the claim that the architecture's layered design
 *   leaves billing-layer commercial arrangements to private ordering,
 *   licensing ISPs to exempt sponsored content from subscribers' data
 *   allowances. Under this reading the standing arrangement — the
 *   sponsorship-exemption economy — is architecturally legitimate yet
 *   observably costly to outsiders: sponsorship fees decouple from service
 *   cost, incumbent platforms purchase reach that unaffiliated rivals'
 *   traffic must consume out of users' scarce allowances, and entry costs
 *   rise for anyone without an exemption budget. The neutrality sibling (far
 *   lower epsilon — non-discrimination read as an architectural requirement
 *   reaching policy) is cited as the authority for prohibiting this
 *   arrangement; the prioritization sibling (intermediate epsilon — managed
 *   quality differentiation as network management) is cited as precedent
 *   normalizing differentiation. Each sibling is a separate constraint story
 *   linked through the network; this file authors epsilon only for the
 *   zero-rating arrangement as this reading sees it — permitted, but
 *   permitted at a price others pay. KEY AGENTS (by structural relationship):
 *   - broadband_isp_operators: Agenda setter (institutional/arbitrage) —
 *   defines caps and exemption categories, collects sponsorship fees -
 *   sponsoring_content_platforms: Primary beneficiary (institutional/mobile)
 *   — buys exemption from data allowances, gains preferential reach -
 *   unsponsored_content_providers: Payer (moderate/constrained) —
 *   functionally identical traffic draws down user allowances -
 *   startup_developers: Payer (powerless/constrained) — entry costs inflated
 *   by the exemption economy - nonprofit_educational_services: Payer/excluded
 *   (powerless/trapped) — mission content penalized, no seat in the
 *   negotiations - end_users_subscribers: Beneficiary/payer
 *   (moderate/constrained) — free sponsored access inside a tilted choice set
 *   - telecom_regulators: Analytical observer (institutional/analytical) —
 *   adjudicates the permission structure - net_neutrality_advocates:
 *   Analytical observer (organized/analytical) — documents effects, mobilizes
 *   opposition
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, 0.68).
domain_priors:suppression_score(tcp_ip_interpretation__zero_rating_reading, 0.6).
domain_priors:theater_ratio(tcp_ip_interpretation__zero_rating_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__zero_rating_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__zero_rating_reading, "Sponsored Data-Cap Exemption Arrangement (Zero-Rating Reading of TCP/IP)").
narrative_ontology:topic_domain(tcp_ip_interpretation__zero_rating_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__zero_rating_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__zero_rating_reading, '106ff627-2ec7-42b9-8015-52c15c5a5df9').
narrative_ontology:cs_kernel_codification('106ff627-2ec7-42b9-8015-52c15c5a5df9', formalized).
narrative_ontology:cs_authority_grounding('106ff627-2ec7-42b9-8015-52c15c5a5df9', expertise).
narrative_ontology:cs_interpretation_layer_present('106ff627-2ec7-42b9-8015-52c15c5a5df9').
narrative_ontology:cs_reading_relation('106ff627-2ec7-42b9-8015-52c15c5a5df9', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('106ff627-2ec7-42b9-8015-52c15c5a5df9', tcp_ip_interpretation__prioritization_reading, influences).
narrative_ontology:cs_axiom('106ff627-2ec7-42b9-8015-52c15c5a5df9', foundational, application_layer_commercial_freedom).
narrative_ontology:cs_axiom_status(application_layer_commercial_freedom, holdable).
narrative_ontology:cs_axiom_grounding('106ff627-2ec7-42b9-8015-52c15c5a5df9', application_layer_commercial_freedom, conventional).
narrative_ontology:cs_axiom('106ff627-2ec7-42b9-8015-52c15c5a5df9', foundational, access_expansion_via_sponsorship).
narrative_ontology:cs_axiom_status(access_expansion_via_sponsorship, holdable).
narrative_ontology:cs_axiom_grounding('106ff627-2ec7-42b9-8015-52c15c5a5df9', access_expansion_via_sponsorship, instrumental).
narrative_ontology:cs_reference_frame('106ff627-2ec7-42b9-8015-52c15c5a5df9', layered_commercial_freedom).
narrative_ontology:cs_drift_state('106ff627-2ec7-42b9-8015-52c15c5a5df9', contemporary_regulatory_record, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('106ff627-2ec7-42b9-8015-52c15c5a5df9', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, broadband_isp_operators).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, sponsoring_content_platforms).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, unsponsored_content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, startup_developers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, nonprofit_educational_services).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, end_users_subscribers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, sponsoring_content_platforms).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, end_users_subscribers).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__zero_rating_reading, layered_architecture_doctrine).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__zero_rating_reading, private_ordering_of_billing_layers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets monthly data allowances, defines which services qualify for exemption, negotiates sponsorship contracts with content platforms, polices traffic classification against allowance accounting, and collects the sponsorship fees. Markets the exemptions to subscribers as perks. Diversified infrastructure owners who can restructure offerings and absorb regulatory setbacks in individual jurisdictions; they carry the regulatory risk when authorities classify the practice as discriminatory pricing.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, broadband_isp_operators, agenda_setter,
    institutional, generational, arbitrage, national).

% Pay sponsorship fees so their services do not draw down subscribers' data allowances. In return their content is effectively free at the point of use while unaffiliated substitutes consume allowance space. They budget the fees as customer-acquisition and retention spending and can withdraw from programs in unfavorable jurisdictions or shift spend across products and markets.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, sponsoring_content_platforms, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, sponsoring_content_platforms, payer).

% Offer services functionally comparable to sponsored ones but without exemption agreements, so their traffic draws down user data allowances while direct rivals' streams free. They compete on product quality against competitors subsidized at the point of consumption. Their options are seeking a sponsorship deal at the operator's discretion, absorbing subscriber churn, or narrowing to audiences insensitive to data costs.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, unsponsored_content_providers, payer,
    moderate, biographical, constrained, global).

% Enter markets where established services are exempt from data charges and theirs are not, so every trial of their product consumes the user's scarce allowance. Customer acquisition costs rise structurally with no exemption available at their scale. They can pivot business models or seek acquisition, but within these markets they operate under a handicap set by other parties' contracts.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, startup_developers, payer,
    powerless, immediate, constrained, national).

% Provide educational, health, and civic information online; their traffic counts fully against user allowances while sponsored entertainment streams free. They have no revenue to bid for inclusion and no seat in the bilateral negotiations that define exemption categories. Their mission binds them to the content they serve, so they cannot pivot away from the penalized traffic class, and their audiences face the steepest relative penalty for reaching them.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, nonprofit_educational_services, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, nonprofit_educational_services, excluded).

% Receive advertised free-data access to sponsored services inside capped plans. The menu of zero-cost usage is chosen by operator-platform deals rather than by them. They bear indirect costs: choice environments tilted toward sponsors, possible recovery of program costs through plan pricing, and program terms that can include tracking. Switching operators is costly where broadband markets are concentrated.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, end_users_subscribers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, end_users_subscribers, payer).

% Adjudicate whether sponsored exemptions constitute discriminatory pricing or legitimate product differentiation. Some jurisdictions have prohibited differential pricing outright after evidentiary proceedings; others have opened and closed inquiries across changes in leadership. They take evidence from every seat and hold the power to redefine the permission structure the whole arrangement depends on.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, telecom_regulators, observer,
    institutional, generational, analytical, national).

% Public-interest organizations and coalitions that document the competitive effects of sponsored exemptions, mobilize mass comment campaigns in regulatory proceedings, and litigate. They operate entirely outside the sponsorship economy, collect none of its revenues, and serve as the channel through which the unorganized payer classes reach decision-makers.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, net_neutrality_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__zero_rating_reading, broadband_isp_operators).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__zero_rating_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Administers scarce network capacity through monthly data allowances and determines, via sponsorship agreements, which traffic draws down those allowances; sponsorship fees fund network operations and give operators a lever for shaping demand on congested networks.
% TRANSFER_FUNCTION: Moves sponsorship payments from large content platforms to network operators; moves user attention and data consumption toward sponsored services by making them free at the point of use; moves competitive position from unaffiliated providers to sponsors, since equivalent unexempt traffic costs users their allowance.
% ABSENT_VOICES: Nonprofit, educational, and independent developers who cannot bid for exemption agreements are absent from the bilateral operator-platform negotiations that define exemption categories; their objection — that identical bytes are priced differently by sponsorship rather than merit — enters the record only through regulatory comment periods, if at all. End users are present only as aggregate marketing segments, never as negotiating parties.
% DISAPPEARANCE_RATIONALE: If sponsored exemptions vanished overnight, sponsorship revenue streams to operators would terminate, sponsored platforms would lose a distribution subsidy their unaffiliated rivals never had, data allowances would bind uniformly across services, and entry barriers built on exemption budgets would fall — pricing, marketing, and competitive strategy across streaming, social, and communications markets would reorganize around uniform cap treatment.
% FOUNDING_PROBLEM: Zero-rating was assembled to solve two problems: making some internet access usable for people who could not afford uncapped data (the access-expansion rationale behind early sponsored packages in low-income markets), and giving carriers a manageable lever over demand on capacity-constrained networks.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: telecommunications regulators in several jurisdictions held evidentiary proceedings on whether the access rationale materialized — one national regulator prohibited differential pricing in 2016 after finding sponsored free packages did not deliver the claimed access expansion; development-economics studies of sponsored package programs found uptake concentrated among existing users of the sponsor's own ecosystem; net-neutrality coalitions documented the entry-barrier effects from outside the arrangement. Operator and platform parties attest the problem is live; the external record treats it as unresolved and, in mature broadband markets, largely superseded by competitive-differentiation motives.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__zero_rating_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__zero_rating_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__zero_rating_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tcp_ip_interpretation__zero_rating_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__zero_rating_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.68) because sponsorship fees are set by bilateral negotiation decoupled from marginal delivery cost, and because the arrangement imposes a competitive externality: identical bytes are priced differently by sponsorship status rather than by network cost, so unaffiliated providers subsidize their rivals' distribution out of their users' allowances. Suppression (0.60) is structural, not internalized: it operates through contract terms, traffic-classification policing, and broadband market concentration that makes ISP-switching costly — there is no cognitive-fusion mechanism to unwind here, so no internalization omega is warranted. Theater ratio (0.42) reflects the gap between the 'free data' and access-expansion framing and the differentiation function the programs actually perform; theater spikes historically when the justification is under regulatory challenge. Accessibility_collapse is moderate-low (0.45): alternatives persist — unexempt access remains possible, entry remains possible at higher cost — nothing approaches natural-law closure. Resistance (0.55) is real and partially successful: differential pricing has been prohibited outright in at least one major jurisdiction, and inquiries elsewhere forced program redesigns.
 *   
 *   The measurement series runs on one shared time grid (t=0,3,6,9,12,15 over 2010–2025) for all three tracked metrics. The trajectories show a regulatory-pressure cycle rather than monotonic drift: extractiveness and suppression climb as packaged exemption programs scale (t0–t6), peak around the 2016 contest — when one national regulator banned differential pricing and another opened a zero-rating inquiry — then partially retreat as the adverse inquiry is closed and the practice consolidates (t9–t15). Theater_ratio peaks at the same contest point because access-expansion rhetoric is deployed defensively exactly when the justification is challenged. The oscillation is driven by the external regulatory-threat cycle, not by intermittent reinforcement as a deliberate mechanism, though the rhetorical surges track threat phases closely. Coalition note: the powerless payer classes (startups, nonprofits) never organized directly; effective coalition power was routed through the analytical seats — advocacy coalitions and mass regulatory-comment campaigns — which is what produced the prohibitions in the record.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the ISP seat the arrangement is voluntary partnership atop genuine capacity management — low experienced cost, collected fees. From the sponsoring-platform seat it is purchased distribution: fees budgeted as acquisition spend, returned as reach rivals cannot match. From the unsponsored-provider and startup seats the same structure is enforced asymmetry with constrained exit — their product quality competes against rivals subsidized at the point of consumption. From the user seat the arrangement sits near symmetric: a genuine perk (free access to popular services) wrapped around a choice set selected by ISP–platform deals rather than by the user. Inter-institutional dynamics: ISP and platform institutions are aligned partners with asymmetric exposures — ISPs collect the fees and carry the regulatory risk; platforms buy the distribution and carry the reputational risk (the consumer backlash that sank one flagship sponsored-access program in a major market). Same-level lateral dynamics: mid-tier unsponsored providers and sponsoring incumbents occupy the same nominal market tier; what differentiates their outcomes is sponsorship budget, not product merit — the constraint converts a commercial-spending difference into a structural access difference.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: broadband_isp_operators and sponsoring_content_platforms sit near the beneficiary end (low d — the arrangement subsidizes them; the ISPs additionally set and enforce it), so effective extraction damps or inverts for their seats. unsponsored_content_providers, startup_developers, and nonprofit_educational_services sit near the full-target end (high d), amplified by constrained and trapped exit — the nonprofits especially, whose mission binds them to the penalized traffic class. end_users_subscribers carry a dual declaration (beneficiary with payer secondary role) placing them near symmetric: real perk, real indirect costs. Observers (regulators, advocates) take analytical seats outside the d-scaling of gain and cost. Suppression is authored as a raw structural property and is deliberately NOT scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope — the global footprint of the sponsoring platforms and the national scope of cap regimes both feed the engine's amplification. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms produce the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is genuinely contested rather than dead: in low-income access contexts the affordability problem the early sponsored packages targeted still exists, while in saturated retail broadband markets the access rationale functions as cover for differentiation rents. Because the status is contested rather than dead, the status-by-verdict mismatch consumer does not fire a zombie flag — and that is the honest reading, not a tuned one. The classification prevents mislabeling in both directions: a pure-snare reading would erase the real coordination substrate (cap administration over finite capacity, which no party disputes is doing work), and a pure-rope reading would erase the sponsorship rent and the entry barriers it raises. The tangled-rope structure holds both facts: genuine coordination function, asymmetric extraction through the same structure, active enforcement required to police exemption boundaries and defend the category system against regulatory redefinition. Mandatrophy is not resolved: the arrangement retains live function while its founding justification fragments by market type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'Is the zero-rating reading — that the TCP/IP architecture''s permission structure extends to sponsored billing exemptions — the correct account of what the kernel entails, or do the sibling readings (neutrality_reading, prioritization_reading) capture what the architecture requires?',
    'Sustained convergence of standards-body statements (IETF), regulatory determinations across jurisdictions, and architectural scholarship on whether the end-to-end principle reaches billing-layer arrangements.',
    'If the neutrality reading prevails, this constraint''s authorization collapses — the arrangement becomes a violation to prohibit rather than a liberty to exercise, and the effective victim set widens to all capped users. If the prioritization reading prevails, zero-rating is subsumed as a billing-layer special case of managed differentiation and its standalone justification weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Committer-frame contingency: this story is one reading of the tcp_ip_interpretation kernel; sibling readings instantiate different constraints with different epsilon and victim sets.').

omega_variable(
    access_expansion_efficacy,
    'Does sponsorship-based zero-rating actually expand meaningful access — first-time connectivity and service diversity for low-income users — or does it primarily redirect existing users deeper into sponsor ecosystems?',
    'Longitudinal adoption studies comparing first-time connectivity and breadth-of-service use in zero-rated markets against matched non-zero-rated markets; regulatory evidentiary records from differential-pricing consultations.',
    'If access expansion fails empirically, the reading''s instrumental axiom loses its warrant and the arrangement reads as competitive exclusion wearing an access costume — pushing classification toward snare. If it succeeds, part of the measured extraction is the price of access expansion and the coordination share is larger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_expansion_efficacy, empirical, 'Whether the access-expansion rationale is empirically real or cover for differentiation rents.').

omega_variable(
    data_cap_substrate_authenticity,
    'Are the data caps that make exemptions valuable genuine capacity-management instruments, or artificially set thresholds that manufacture the scarcity the exemption economy monetizes?',
    'Disclosure or independent measurement of network utilization against engineering capacity thresholds; comparison of cap levels across ISPs with similar infrastructure cost structures.',
    'If caps are artificial, the exemption economy monetizes a self-created shortage — shifting classification toward snare and raising effective extraction. If caps track real congestion management, the coordination substrate is genuine and the tangled-rope structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_cap_substrate_authenticity, empirical, 'Authenticity of the cap substrate the sponsorship market prices against.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__zero_rating_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(tcp__tr_t0, observed).
narrative_ontology:measurement(tcp__tr_t3, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 3, 0.34).
narrative_ontology:measurement_basis(tcp__tr_t3, observed).
narrative_ontology:measurement(tcp__tr_t6, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 6, 0.46).
narrative_ontology:measurement_basis(tcp__tr_t6, observed).
narrative_ontology:measurement(tcp__tr_t9, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 9, 0.41).
narrative_ontology:measurement_basis(tcp__tr_t9, observed).
narrative_ontology:measurement(tcp__tr_t12, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement_basis(tcp__tr_t12, observed).
narrative_ontology:measurement(tcp__tr_t15, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement_basis(tcp__tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(tcp__be_t0, observed).
narrative_ontology:measurement(tcp__be_t3, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 3, 0.54).
narrative_ontology:measurement_basis(tcp__be_t3, observed).
narrative_ontology:measurement(tcp__be_t6, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement_basis(tcp__be_t6, observed).
narrative_ontology:measurement(tcp__be_t9, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 9, 0.66).
narrative_ontology:measurement_basis(tcp__be_t9, observed).
narrative_ontology:measurement(tcp__be_t12, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement_basis(tcp__be_t12, observed).
narrative_ontology:measurement(tcp__be_t15, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(tcp__be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(tcp__su_t0, observed).
narrative_ontology:measurement(tcp__su_t3, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 3, 0.5).
narrative_ontology:measurement_basis(tcp__su_t3, observed).
narrative_ontology:measurement(tcp__su_t6, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 6, 0.64).
narrative_ontology:measurement_basis(tcp__su_t6, observed).
narrative_ontology:measurement(tcp__su_t9, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 9, 0.57).
narrative_ontology:measurement_basis(tcp__su_t9, observed).
narrative_ontology:measurement(tcp__su_t12, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 12, 0.59).
narrative_ontology:measurement_basis(tcp__su_t12, observed).
narrative_ontology:measurement(tcp__su_t15, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement_basis(tcp__su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__zero_rating_reading, resource_allocation).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__prioritization_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'what TCP/IP allows.' Three readings of one kernel, three stories, three epsilons: the neutrality reading (low epsilon — non-discrimination as architectural requirement; cited as the authority for prohibiting this arrangement), the prioritization reading (intermediate epsilon — managed quality differentiation; cited as precedent normalizing differentiation), and this zero-rating reading (higher epsilon — sponsored billing exemptions advantaging incumbents and raising entry barriers). The upstream neutrality claim is invoked against this arrangement; the lateral prioritization claim is invoked to legitimize it. Each member carries its own stable epsilon, beneficiary set, and victim set; this file authors only the zero-rating arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
