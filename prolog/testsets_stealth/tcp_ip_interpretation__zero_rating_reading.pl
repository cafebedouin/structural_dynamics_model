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
 *   constraint_id: tcp_ip_interpretation__zero_rating_reading
 *   human_readable: Sponsored Data Exemption Regime (Zero-Rating Reading of the TCP/IP Kernel)
 *   domain: technology governance/telecommunications law
 *
 * SUMMARY:
 *   Under the zero-rating reading of the TCP/IP kernel, the architecture's
 *   content-agnostic transport layer leaves application-layer treatment of
 *   traffic to commercial design: network operators may exempt selected
 *   services from metered data caps when content providers sponsor the
 *   exemption or when the operator bundles its own services. The standing
 *   arrangement this story is about is the sponsored-exemption regime itself
 *   — sponsorship contracts, cap administration, exemption lists — which
 *   advantages incumbent platforms that can afford sponsorship, raises the
 *   effective cost of user acquisition for unaffiliated developers, and
 *   converts the data cap from a flat cost into a steering instrument. The
 *   coordination half is real: sponsored access lets users reach high-value
 *   services without data charges, a genuine affordability mechanism in
 *   markets where metered prices price out consumption. The extraction half
 *   rides the same structure: privileged reach is sold to incumbents and
 *   everyone else's traffic is taxed by the cap. This file instantiates ONE
 *   reading of a contested kernel; the sibling readings (neutrality_reading,
 *   prioritization_reading) are separate constraints with their own ε,
 *   beneficiary structures, and classifications — the contest is recorded in
 *   omega variables, not averaged into this one. The claim/metric gap is
 *   deliberate: claimed_type is the structurally true hybrid; the metrics
 *   describe the arrangement's actual operation.
 *
 * KEY AGENTS:
 *   - - mobile_network_operators: agenda setter and direct collector (institutional/arbitrage) — administers caps and exemption lists, collects sponsorship payments, could dissolve the arrangement at will
 *   - - incumbent_platform_sponsors: primary beneficiary (powerful/arbitrage) — buys cap-free reach at a scale entrants cannot match, gains an engagement moat across many national markets
 *   - - sponsored_service_users: beneficiary with a secondary payer position (powerless/constrained) — free access to exempted services, full cap exposure to everything else, steered choice set
 *   - - price_sensitive_data_users: payer (powerless/constrained) — smallest plans, hardest cap bind, widest free-vs-metered gap, no unlimited-plan exit
 *   - - independent_developers: payer (moderate/constrained) — metered competition against exempt incumbents, sponsorship priced beyond reach, open web as a partial and disadvantaged alternative
 *   - - neutrality_regulators: analytical observer (institutional/analytical) — consults, measures, prohibits or conditions; their decisions reprice or dissolve the sponsorship channel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, 0.66).
domain_priors:suppression_score(tcp_ip_interpretation__zero_rating_reading, 0.52).
domain_priors:theater_ratio(tcp_ip_interpretation__zero_rating_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__zero_rating_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__zero_rating_reading, "Sponsored Data Exemption Regime (Zero-Rating Reading of the TCP/IP Kernel)").
narrative_ontology:topic_domain(tcp_ip_interpretation__zero_rating_reading, "technology governance/telecommunications law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__zero_rating_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__zero_rating_reading, 'e32dd9e6-569a-45fb-b4fa-70aa91921859').
narrative_ontology:cs_kernel_codification('e32dd9e6-569a-45fb-b4fa-70aa91921859', distributed).
narrative_ontology:cs_authority_grounding('e32dd9e6-569a-45fb-b4fa-70aa91921859', distributed).
narrative_ontology:cs_reading_relation('e32dd9e6-569a-45fb-b4fa-70aa91921859', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('e32dd9e6-569a-45fb-b4fa-70aa91921859', tcp_ip_interpretation__prioritization_reading, influences).
narrative_ontology:cs_axiom('e32dd9e6-569a-45fb-b4fa-70aa91921859', foundational, content_blind_transport_permits_selective_exemption).
narrative_ontology:cs_axiom_status(content_blind_transport_permits_selective_exemption, holdable).
narrative_ontology:cs_axiom_grounding('e32dd9e6-569a-45fb-b4fa-70aa91921859', content_blind_transport_permits_selective_exemption, empirically_contingent).
narrative_ontology:cs_axiom('e32dd9e6-569a-45fb-b4fa-70aa91921859', foundational, sponsored_exemption_expands_access).
narrative_ontology:cs_axiom_status(sponsored_exemption_expands_access, holdable).
narrative_ontology:cs_axiom_grounding('e32dd9e6-569a-45fb-b4fa-70aa91921859', sponsored_exemption_expands_access, instrumental).
narrative_ontology:cs_reference_frame('e32dd9e6-569a-45fb-b4fa-70aa91921859', content_agnostic_transport_commercial_layering).
narrative_ontology:cs_drift_state('e32dd9e6-569a-45fb-b4fa-70aa91921859', post_neutrality_regulation_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e32dd9e6-569a-45fb-b4fa-70aa91921859', '2026-08-03T00:00:00Z').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, mobile_network_operators).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, incumbent_platform_sponsors).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, sponsored_service_users).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, independent_developers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, price_sensitive_data_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, sponsored_service_users).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__zero_rating_reading, content_blind_transport_doctrine).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__zero_rating_reading, sponsored_access_affordability_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the size and structure of monthly data caps, negotiate sponsorship agreements with content providers, maintain the exemption list that decides which services do not draw down the cap, and enforce the boundary technically through traffic identification and contractually through terms barring circumvention. Collect the sponsorship payments directly. Could unwind the entire arrangement by retiring caps or opening exemptions to all comers; nothing in the network's operation requires the selective form.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, mobile_network_operators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, mobile_network_operators, beneficiary).

% Operate the large consumer platforms whose services are exempted. Pay sponsorship fees sized to their scale, which smaller services cannot match, and gain the engagement that follows when using the service costs the user no data. Present in many national markets at once, so a fee that is routine for them is out of reach for a new entrant in any single one of them.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, incumbent_platform_sponsors, beneficiary,
    powerful, generational, arbitrage, global).

% Use zero-rated services — messaging, social platforms, streaming bundles — without the use drawing down their data allowance. The cap applies in full to everything not on the exemption list, so the same monthly purchase buys far more of the sponsored services than of anything else. Switching carriers means new contracts, devices, and numbers; within a carrier, the choice set is whatever the exemption list contains.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, sponsored_service_users, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, sponsored_service_users, payer).

% Hold the smallest data plans, where the cap binds hardest and top-up or overage costs are material relative to income. Face the widest gap between free (sponsored) and metered (everything else): local news, government services, small e-commerce, and any new service consume the allowance while large sponsored platforms do not. Cannot buy their way out with unlimited plans.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, price_sensitive_data_users, payer,
    powerless, biographical, constrained, national).

% Build and operate services outside the sponsorship channel. Their traffic draws down users' caps while exempted rivals' does not, raising the effective price of trying their product. Sponsorship pricing is out of reach at their scale; the open web remains available, but discovery, habits, and defaults run through the zero-rated apps.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, independent_developers, payer,
    moderate, biographical, constrained, global).

% Sectoral consultation, measurement, and rule-making bodies that evaluate whether sponsored exemptions distort competition and degrade the openness of the network. Some have prohibited commercial zero-rating outright, others have conditioned it; their decisions reprice or dissolve the sponsorship channel within their jurisdictions.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, neutrality_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__zero_rating_reading, mobile_network_operators).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__zero_rating_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts a flat metered cost of connectivity into a sponsored allocation: users reach designated high-value services without drawing down a scarce, priced allowance; operators recover costs from sponsors rather than users; platforms acquire users in markets where data prices are the binding adoption constraint. The cap itself remains the capacity-management instrument around which the exemptions are carved.
% TRANSFER_FUNCTION: Moves sponsorship payments from large content platforms to network operators; moves user attention and engagement toward exempted services and away from metered ones; moves the cost of unsponsored data consumption onto users' capped allowances and, through reduced competitive pressure, onto the prices and quality of the broader app market.
% ABSENT_VOICES: Unaffiliated developers and would-be entrants have no seat in sponsorship negotiations — deals are struck between operators and incumbents, with entrants affected but never party. In several deployments the regulator learned of exemptions only after launch. Future users not yet in the market bear the entry-barrier effect and are represented by no one at the table.
% DISAPPEARANCE_RATIONALE: If sponsored exemptions vanished overnight, sponsorship revenue to operators stops, incumbents lose cap-free reach and their engagement advantage compresses, developers' traffic competes on equal metered terms, caps revert to a flat cost or are repriced, and in markets where the exemption was the adoption channel, platform use patterns visibly shift within months — the arrangement's beneficiaries reorganize around its absence rather than reproducing it.
% FOUNDING_PROBLEM: Metered mobile data was expensive relative to incomes in many markets, capping consumption and blocking platform adoption; operators simultaneously needed a way to charge for capacity without pricing low-income users out entirely. Sponsored exemption was built to let platforms underwrite users' access to their services and let operators monetize capacity through sponsors.
% FOUNDING_PROBLEM_CORROBORATION: Development-economics literature and operator disclosures corroborate that data prices were a real adoption barrier in low-income markets — the affordability problem is live where it was built for. Sectoral regulators' consultation records (India's 2015–16 process; the EU's open-internet guideline work) corroborate, from outside the benefiting parties, that congestion-management justifications frequently did not match cap design, and competition analyses document the entry-barrier effect. No party outside the operator–sponsor set attests that the arrangement's current selective form is required by the founding problem.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__zero_rating_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__zero_rating_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__zero_rating_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tcp_ip_interpretation__zero_rating_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__zero_rating_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored at 0.66: the sponsorship fee is decoupled from any marginal network cost of the exempted traffic — the traffic still traverses the network; what is sold is relief from the cap — and the cap's steering effect concentrates engagement on incumbents while raising entrants' user-acquisition costs. Suppression is 0.52: enforcement is contractual and technical (cap administration, exemption-list maintenance, anti-circumvention terms) plus switching friction for users; coercion is real but bounded, since wifi, carrier switching, and regulation remain partial exits. Theater is 0.40: the stated justifications are congestion management and affordability; the affordability half is genuinely real in low-income markets, but a growing share of cap maintenance persists in networks and eras where congestion does not bind, where the cap functions as a revenue and steering instrument. Accessibility_collapse is 0.45: alternatives do not fully collapse — regulators have prohibited the practice outright in major markets, users retain wifi and costly carrier switching, developers retain the open web — but each alternative is partial, and the sponsorship channel itself is not replicable by entrants. Resistance is 0.60: sustained neutrality coalitions, startup opposition, and regulator action (India's prohibition, EU conditioning). Notably, the powerless user seats exercised coalition power through mass public consultations — the mechanism that produced the prohibitions — which is why resistance is high despite individually weak seats. Claimed type is tangled_rope: a real allocation/affordability problem is solved (coordination half) while asymmetric advantage is sold through the same structure (extraction half), and the arrangement requires active enforcement — caps must be administered and exemption lists maintained contract by contract. The measurement series share one grid; the mild plateau at t=16–20 reflects regulatory pushback capping the practice in some markets while it spreads in others.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structural data. From the operator seat the regime is a resource-allocation product it designed, prices, and could unwind — allocation with a revenue model. From the incumbent-sponsor seat it is customer acquisition at a price entrants cannot match — an advantage, not a burden. From the sponsored-user seat the same structure is mostly free services, with the steering visible only at the margin of the cap. From the developer and price-sensitive-user seats it is a tax on unsponsored traffic and a wall in front of entry. The regulator seat sees a discrimination question about the network itself. No seat is wrong about its own position; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Operators and incumbent sponsors sit at the beneficiary end: both collect (fees, engagement moats) with arbitrage-grade exit — the operator administers the arrangement and could dissolve it; the sponsor multi-homes across markets and can shift spend. Sponsored users derive damped treatment: primary beneficiary, secondary payer — the cap taxes precisely the consumption the exemption does not cover, so their effective position sits above a pure beneficiary's. Price-sensitive users and independent developers sit near the target end: they bear the transfer (steered caps, metered competition) with constrained exits — sponsorship is priced beyond them and switching carries real friction. The regulator is the analytical seat. Scope amplification applies modestly: the arrangement operates at national scale per carrier with global platform sponsors, so verification of cross-market effects is harder than the per-deal view suggests. No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the correct d for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what prevents mislabeling in both directions. A snare reading would erase the genuine coordination half — in markets where metered data prices out first-time users, sponsored access demonstrably expanded what people could reach, and reading the arrangement's persistence as pure coercion would misdescribe why users accepted it. A rope reading would erase the extraction half — the sponsorship channel is not open to entrants, the cap taxes unsponsored traffic, and the affordability benefit doubles as an incumbent lock-in. The R5 genealogy keeps the record honest without adjudicating it: the founding problem is contested — the affordability problem is live where it was real (low-income, low-coverage markets), while the congestion-management justification is largely dead in oversubscribed networks; the mismatch consumer can check that contested status against the world_rearranges verdict and against the theater_ratio trajectory, which rises as the congestion justification decays while cap administration persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_delta,
    'This story is the zero_rating_reading of the tcp_ip_interpretation kernel. What structural delta would each sibling reading produce if instantiated instead of this one?',
    'Compare against the sibling constraint files (tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__prioritization_reading). Under the neutrality_reading the exemption regime is a violation with a pure victim structure and no coordination credit; under the prioritization_reading exemptions are one instance of a general differentiation permission with a different beneficiary set (quality tiers rather than cap exemptions). The disagreement is located in whether the transport layer''s content-blindness carries a non-discrimination norm or a design-freedom default.',
    'The classification of the standing practice flips with the reading: prohibition-side victim structure under the neutrality_reading, generalized permissibility under the prioritization_reading, hybrid coordination-plus-extraction under this reading. Cross-reading comparison is valid only through the family links, never by averaging ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings change the victim/beneficiary structure and classification of the same practice.').

omega_variable(
    access_onramp_vs_steering,
    'In low-income, low-coverage markets, does sponsored exemption net-expand access (an affordability on-ramp) or net-distort consumption toward sponsors (a steering effect with persistent lock-in)?',
    'Longitudinal adoption and consumption studies comparing zero-rating markets with matched control markets, including what users do once income or coverage rises — whether they diversify beyond sponsored services or remain inside them.',
    'Net-expansion strengthens the coordination half and holds effective extraction down; net-steering with persistent lock-in shifts the arrangement toward the snare boundary and raises effective extraction for the user seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_onramp_vs_steering, empirical, 'Whether the affordability benefit is real net access expansion or the visible half of a steering mechanism.').

omega_variable(
    cap_congestion_reality,
    'Are the data caps that make exemptions valuable actually congestion- or cost-bound where deployed, or are they revenue instruments?',
    'Network utilization and oversubscription economics per market: caps binding at congestion-relevant thresholds support the capacity-management framing; caps set well above congestion relevance, or retained unchanged after capacity upgrades, expose the revenue and steering function.',
    'If caps are revenue instruments, the theater share is understated at 0.40, the justification structure degrades toward pure gatekeeping, and the arrangement drifts toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cap_congestion_reality, empirical, 'Whether the cap — the instrument the exemptions modify — performs its stated function.').

omega_variable(
    sponsor_concentration_threshold,
    'Does the extraction profile depend on sponsor concentration — is zero-rating in single-dominant-platform markets effectively exclusive dealing, while multi-sponsor markets approximate a competitive market for exemptions?',
    'Per-market measurement of sponsor concentration (share of exempted traffic held by the top sponsor) and exclusivity terms in sponsorship contracts.',
    'High concentration with exclusivity terms pushes the arrangement toward exclusionary extraction concentrated on one platform''s rivals; dispersed sponsorship supports the resource-allocation framing and keeps the hybrid classification stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sponsor_concentration_threshold, empirical, 'Whether market structure, not the exemption mechanism itself, determines how extractive the arrangement is.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__zero_rating_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(tcp__tr_t0, observed).
narrative_ontology:measurement(tcp__tr_t4, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement_basis(tcp__tr_t4, observed).
narrative_ontology:measurement(tcp__tr_t8, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(tcp__tr_t8, observed).
narrative_ontology:measurement(tcp__tr_t12, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement_basis(tcp__tr_t12, observed).
narrative_ontology:measurement(tcp__tr_t16, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement_basis(tcp__tr_t16, observed).
narrative_ontology:measurement(tcp__tr_t20, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(tcp__tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(tcp__be_t0, observed).
narrative_ontology:measurement(tcp__be_t4, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 4, 0.54).
narrative_ontology:measurement_basis(tcp__be_t4, observed).
narrative_ontology:measurement(tcp__be_t8, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement_basis(tcp__be_t8, observed).
narrative_ontology:measurement(tcp__be_t12, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(tcp__be_t12, observed).
narrative_ontology:measurement(tcp__be_t16, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement_basis(tcp__be_t16, observed).
narrative_ontology:measurement(tcp__be_t20, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(tcp__be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(tcp__su_t0, observed).
narrative_ontology:measurement(tcp__su_t4, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement_basis(tcp__su_t4, observed).
narrative_ontology:measurement(tcp__su_t8, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement_basis(tcp__su_t8, observed).
narrative_ontology:measurement(tcp__su_t12, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement_basis(tcp__su_t12, observed).
narrative_ontology:measurement(tcp__su_t16, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement_basis(tcp__su_t16, observed).
narrative_ontology:measurement(tcp__su_t20, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(tcp__su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__zero_rating_reading, resource_allocation).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__prioritization_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what TCP/IP requires of the network' decomposes into three structurally distinct readings of one kernel, each with its own ε, beneficiary/victim structure, and classification. This file is the zero_rating_reading (sponsored cap exemptions authorized). The neutrality_reading (non-discrimination required; exemptions are violations) and the prioritization_reading (differentiated quality permitted as network management) are separate stories. The upstream claim each sibling cites is the same architecture; the readings differ on whether content-blindness carries a non-discrimination norm or a design-freedom default, so their ε values diverge by construction. All three files link one another via network.affects_constraints; ε is never averaged across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
