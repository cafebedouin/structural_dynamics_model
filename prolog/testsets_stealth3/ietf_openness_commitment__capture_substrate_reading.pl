% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__capture_substrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__capture_substrate_reading, []).

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
 *   constraint_id: ietf_openness_commitment__capture_substrate_reading
 *   human_readable: IETF Openness Commitment — Capture Substrate Reading
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   The IETF openness commitment — 'rough consensus and running code,' open
 *   mailing lists, individual participation, freely available specifications
 *   — is here instantiated as ONE reading of a contested kernel: the
 *   capture-substrate reading. On this reading the standards process
 *   genuinely solves a coordination problem (multi-vendor interoperability),
 *   and precisely because it does, resource advantage converts into encoded
 *   gatekeeping: well-resourced platform operators staff working groups,
 *   supply chairs and area directors, steer requirements toward their
 *   architectures, and ship proprietary extensions that fragment the commons
 *   they helped specify, while royalty-bearing patent terms convert published
 *   openness into per-implementer tolls. Small implementers, open-source
 *   developers, and end users bear the resulting costs. Per the
 *   epsilon-invariance principle this file authors ONLY this reading as a
 *   clean, epsilon-stable constraint: the commons-stewardship and
 *   legitimacy-erosion readings are separate constraints (separate files)
 *   linked through network.affects_constraints, not folded into this one.
 *   Interval mapping: t=0 corresponds to January 1986 (first IETF meeting,
 *   roughly twenty attendees, academic and volunteer era); t=38 corresponds
 *   to 2024 (hybrid meetings, corporate-dominated attendance, mature
 *   intellectual-property machinery). Claim and metrics are independent
 *   authored facts: claimed_type states the structural reading (real
 *   coordination plus asymmetric extraction plus active enforcement); the
 *   metrics describe observed operation; the engine computes per-seat
 *   classifications from the structural data.
 *
 * KEY AGENTS:
 *   - large_platform_operators: Primary beneficiary (institutional/arbitrage) — converts participation budgets into specification control and extension headroom
 *   - ietf_leadership_iesg_wg_chairs: Agenda setter (institutional/mobile) — administers the consensus procedure; the chair pipeline draws heavily from operator employers
 *   - patent_holding_firms: Secondary beneficiary (powerful/arbitrage) — embeds essential claims in published specifications, collects per-implementer royalties
 *   - small_implementers: Primary target (moderate/constrained) — bears royalty and reimplementation costs
 *   - open_source_developers: Target with partial identity lock (moderate/identity_locked) — bears costs while bound to the open-process ethos
 *   - protocol_end_users: Near-symmetric seat (organized/constrained) — receives the interoperability subsidy, pays fragmentation and licensing pass-through
 *   - resource_constrained_regional_implementers: Excluded voice (powerless/trapped) — priced out of participation, objects from outside the room
 *   - interoperability_policy_researchers: Analytical observer (analytical/analytical) — measures authorship concentration and licensing-term prevalence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, 0.58).
domain_priors:suppression_score(ietf_openness_commitment__capture_substrate_reading, 0.47).
domain_priors:theater_ratio(ietf_openness_commitment__capture_substrate_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__capture_substrate_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__capture_substrate_reading, "IETF Openness Commitment — Capture Substrate Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__capture_substrate_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__capture_substrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__capture_substrate_reading, '0c7982d7-be31-4785-9f9e-1db6f83c9912').
narrative_ontology:cs_kernel_codification('0c7982d7-be31-4785-9f9e-1db6f83c9912', distributed).
narrative_ontology:cs_authority_grounding('0c7982d7-be31-4785-9f9e-1db6f83c9912', practice).
narrative_ontology:cs_interpretation_layer_present('0c7982d7-be31-4785-9f9e-1db6f83c9912').
narrative_ontology:cs_reading_relation('0c7982d7-be31-4785-9f9e-1db6f83c9912', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c7982d7-be31-4785-9f9e-1db6f83c9912', ietf_openness_commitment__legitimacy_erosion_reading, influences).
narrative_ontology:cs_axiom('0c7982d7-be31-4785-9f9e-1db6f83c9912', foundational, resource_asymmetry_voids_procedural_openness).
narrative_ontology:cs_axiom_status(resource_asymmetry_voids_procedural_openness, holdable).
narrative_ontology:cs_axiom_grounding('0c7982d7-be31-4785-9f9e-1db6f83c9912', resource_asymmetry_voids_procedural_openness, empirically_contingent).
narrative_ontology:cs_axiom('0c7982d7-be31-4785-9f9e-1db6f83c9912', secondary, extension_control_tracks_participation_investment).
narrative_ontology:cs_axiom_status(extension_control_tracks_participation_investment, holdable).
narrative_ontology:cs_axiom_grounding('0c7982d7-be31-4785-9f9e-1db6f83c9912', extension_control_tracks_participation_investment, empirically_contingent).
narrative_ontology:cs_reference_frame('0c7982d7-be31-4785-9f9e-1db6f83c9912', nominal_open_participation_consensus).
narrative_ontology:cs_drift_state('0c7982d7-be31-4785-9f9e-1db6f83c9912', post_commercialization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0c7982d7-be31-4785-9f9e-1db6f83c9912', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, patent_holding_firms).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, small_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, open_source_developers).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, protocol_end_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, protocol_end_users).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__capture_substrate_reading, rough_consensus_procedure_doctrine).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__capture_substrate_reading, running_code_validation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the consensus machinery: working-group chairs manage mailing-list discussion and judge when consensus is reached; Area Directors review and approve specifications for publication. Most serve part-time while employed by the larger participating organizations, and the pipeline of experienced chairs draws disproportionately from well-resourced delegations. Their exit is ordinary job mobility — stepping down from a leadership role or changing employers does not remove them from the industry.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, ietf_leadership_iesg_wg_chairs, agenda_setter,
    institutional, biographical, mobile, global).

% Send delegations of engineers to working groups, fund repeated international travel, host mailing-list infrastructure, and employ many of the most experienced protocol designers. That investment buys durable influence over requirements, architecture choices, and extension points. Because they operate the largest deployed implementations, they can ship extensions ahead of or beyond published specifications and watch the market follow. If a given venue turns hostile they can shift weight to friendlier forums, join or launch rival consortia, or simply deploy de facto — leaving costs them little.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, large_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Hold portfolios of patents that read on published specifications, disclosed through the intellectual-property machinery and licensed on royalty-bearing terms. Every implementer of the standard becomes a licensing customer. Their returns do not depend on running the process, only on the specifications continuing to be implemented; they can pursue equivalent income in any forum that publishes implementable specifications.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, patent_holding_firms, beneficiary,
    powerful, generational, arbitrage, global).

% Build products on published specifications with engineering teams far smaller than the large delegations. They attend meetings rarely, comment on mailing lists between shipping cycles, and absorb royalty obligations per unit shipped. When a large operator ships a proprietary extension that fragments the specification, they must either reimplement it under uncertainty or lose compatibility with the dominant installed base. Leaving the standards ecosystem entirely would mean abandoning interoperability with the platforms their customers use.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, small_implementers, payer,
    moderate, biographical, constrained, global).

% Maintain royalty-free implementations of published specifications, often unpaid or volunteer-funded. Participation costs fall on personal time; patent obligations fall on the projects' users or force feature withdrawal. Their professional standing and community identity are built around the open-process ethos — showing up, reviewing, implementing — which keeps them inside a process whose economics favor their better-funded counterparts even when individual projects suffer. Exit would mean not only losing influence but abandoning the identity that organizes their work.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, open_source_developers, payer,
    moderate, biographical, identity_locked, global).

% Receive the interoperability subsidy: devices, browsers, and services that speak the same protocols. They pay indirectly where extension-driven fragmentation reaches them — broken features, degraded privacy defaults, ecosystem lock-in — and directly where licensing costs are passed through in product prices. Their aggregated market choices exert real pressure, but no individual user can evaluate or act on specification-level decisions.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, protocol_end_users, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__capture_substrate_reading, protocol_end_users, payer).

% Would-be implementers in universities, small firms, and public institutions outside the major technology hubs. Meeting costs, visa friction, and time-zone-hostile scheduling price them out of synchronous participation; mailing-list latency further disadvantages them in fast-moving debates. They inherit whatever licensing terms and architectural choices the seated participants settle on, with no procedural standing to contest them.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, resource_constrained_regional_implementers, excluded,
    powerless, biographical, trapped, continental).

% Study the standards system from outside: measuring authorship concentration, tracking licensing-term prevalence, and documenting fragmentation episodes. They publish analyses that other seats cite but hold no vote in any working group and control no enforcement lever.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, interoperability_policy_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__capture_substrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the multi-vendor interoperability problem: produces shared, openly published specifications so independently built systems interoperate without any single vendor owning the wire format; concentrates security review and conformance expectations in one venue; gives implementers a stable reference point for long-lived infrastructure.
% TRANSFER_FUNCTION: Moves specification control and implementation-cost burden according to participation capacity: engineer-hours, travel budgets, and legal spend convert into requirements influence and chair positions; royalty-bearing patent terms move per-unit payments from implementers to patent holders; extension headroom moves market-defining discretion to the largest deployers; visibility and professional standing flow to well-resourced delegations.
% ABSENT_VOICES: Resource-constrained implementers outside major hubs, end users, and future market entrants locked out by royalty-bearing terms would object if present; they are absent because participation itself is the priced input — the people the gatekeeping falls hardest on are the least able to sit in the room where it is negotiated.
% DISAPPEARANCE_RATIONALE: If the process and its openness commitment vanished overnight, protocol evolution would reorganize within years around de facto single-vendor specifications and closed consortia; interoperability would persist only where a dominant vendor permitted it, and the multi-vendor internet application layer would fragment along operator boundaries.
% FOUNDING_PROBLEM: Heterogeneous computer networks in the 1970s-80s could not interoperate: each vendor and research network ran private protocols, and connecting them required per-pair gateway engineering. The arrangement was built to produce common protocols through open collaboration among rivals, without a central owner.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists outside the beneficiary set: academic network-measurement groups document recurring interoperability failures in newly standardized layers; independent open-source foundations attest that emerging domains still need common specifications; standards-policy researchers publish participation-asymmetry audits. No attestation from the benefiting operators is relied upon.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__capture_substrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__capture_substrate_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__capture_substrate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ietf_openness_commitment__capture_substrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__capture_substrate_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__capture_substrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ietf_openness_commitment__capture_substrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores are authored independently of the claim. Extractiveness 0.58: the standing arrangement transfers specification control and per-unit royalty burden according to participation capacity, while every seat retains the interoperability subsidy — moderate, not total. Suppression 0.47 is structural throughout: cost barriers, procedural gatekeeping, and network-effect lock-in; nothing here is internalized belief, so no structural-versus-internalized omega is required. Theater 0.42: openness rituals (calls for participation, consensus affirmations) increasingly outrun the access they advertise. Accessibility collapse 0.52: once a specification wins deployment, proprietary forks and rival venues remain possible but costly, so alternatives narrow without vanishing. Resistance 0.55: documented revolt episodes — licensing-term reversals, fork defections, rival consortia — meet the arrangement repeatedly without displacing it. The temporal series run on one shared grid (t = 0, 6, 12, 19, 25, 31, 38, mapping to 1986-2024) with all three tracked metrics authored at every point. The suppression_requirement series is authored deliberately: this story traces enforcement-capacity change — gatekeeping machinery hardened as commercial stakes rose (approval formalization, intellectual-property disclosure regimes, chair professionalization) — which is exactly the dynamic that metric exists to carry; a static scalar would hide the ratchet. Base extractiveness accumulates with commercialization; theater climbs as openness rhetoric intensifies against narrowing access. Payer-coalition episodes (small vendors, open-source projects, and civil-society groups forcing a licensing-term reversal in 2003) appear in the record as temporary resistance spikes that the annualized series smooths over.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is structural, not rhetorical. From the operator seat the process is a meritocracy it generously funds: it pays engineers to show up, and influence follows contribution — the same facts this reading codes as gatekeeping are, from inside, evidence of seriousness. From the small-implementer and open-source seats the identical mechanics operate as a toll booth: the specification is free, the participation is not, and the extension arrives after the specification is frozen. The leadership seat experiences procedural integrity — every objection answered, every decision documented — while administering outcomes shaped by who was resourced to object. Identity-lock dynamics bind the open-source seat specifically: the open-process ethos is constitutive of professional and community identity, so exit carries an identity cost beyond the economic one; the lock is partial (contributors do leave for proprietary work) but persistent, and if the frame broke — if the community broadly concluded the process is already captured — the predicted behavior is mass realignment toward alternative venues or fork culture rather than quiet exit. Coalition potential among payer seats is real but episodic: the 2003 royalty-free revolt showed small vendors, open-source projects, and civil-society groups forcing licensing-term reversal, yet such coalitions dissolve once the immediate term fight ends, leaving the standing asymmetry intact.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations map directly onto the structural relationships. Large platform operators sit near the subsidized pole: the arrangement subsidizes their influence acquisition and extension freedom, and their arbitrage-grade exit damps any cost they do bear. Patent-holding firms likewise collect without bearing. Small implementers and open-source developers sit near the full-target pole — they pay per-unit royalties, reimplementation costs, and participation costs they cannot recoup; the open-source seat's identity-locked exit pushes it toward the extreme target end, since the lock removes even the exit modulation a mobile payer would enjoy. Protocol end users are genuinely dual-positioned: the coordination subsidy is real and large, but fragmentation and licensing pass-through reach them diffusely. One directionality override is authored: the organized power atom (held in this story only by protocol_end_users) is set to d=0.5 because the derivation reads the declared primary beneficiary role and would place end users near the subsidized pole, ignoring the secondary payer position that puts their net position near symmetric. All other seats derive correctly from role plus exit data, so no other overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline cuts both ways here. Against the pure-extraction misread: the coordination function is genuine and load-bearing — multi-vendor interoperability is produced and consumed by every seat, including the payers — so the extraction riding on it is not cover for a purely predatory arrangement, and the victim set is partly compensated by the subsidy. Against the clean-coordination misread: the transfer function is real and asymmetric, enforcement is active (chair control, approval gates, intellectual-property machinery), and alternatives are suppressed at the margin (extension preemption, licensing tolls), so the arrangement is not a costless coordination solution. The hybrid classification keeps both halves visible. Mandatrophy is not resolved: the founding problem (heterogeneous-network interoperability) is live, corroborated from outside the beneficiary set, and the six-questions mismatch consumer should find status=live paired with verdict=world_rearranges — no zombie flag. Theater (0.42) is elevated but symptomatic of openness rhetoric outrunning access, not of an atrophied function; the process still ships specifications the world implements. Receipt is consolidated: control rents and patent royalties land predominantly on the same corporate seats (operator patent portfolios overlap the royalty-collection seat), which is why gain_flow names large_platform_operators rather than diffuse. Fixing cost is authored prohibitive: the administering body depends on the corporate resourcing that constitutes the capture channel, and reform that removed it — funded universal participation, mandatory royalty-free terms — would threaten the process's own material base faster than the diffuse harms pressuring it; the fixer bears the cost of fixing while the harm of not fixing falls elsewhere.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This file instantiates the capture-substrate reading of the ietf_openness_commitment kernel; is the standing arrangement better described by this reading or by a sibling — commons_stewardship_reading (genuine open infrastructure preserving interoperability) or legitimacy_erosion_reading (the rough-consensus mechanism itself as the contested, capture-vulnerable object) — and where exactly do the readings part ways?',
    'Cross-reading comparison on shared observables: authorship and chair-affiliation concentration, extension licensing-term prevalence, and consensus-contention case histories. The disagreement is located in whether resource-derived control is incidental friction around a functioning commons (stewardship) or the operative selection mechanism that openness rhetoric conceals (this reading).',
    'Under the stewardship reading epsilon drops toward rope-like levels with no named victim set; under the erosion reading the contested object shifts to the consensus procedure and its safeguards; under this reading the arrangement is a hybrid with real coordination plus asymmetric extraction — operators as beneficiaries, implementers and users as payers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer-frame omega: which reading of the openness kernel describes the arrangement, and where the readings structurally diverge.').

omega_variable(
    participation_control_attribution,
    'What fraction of durable specification control (authorship of normative text, chair and Area Director positions, requirements decisions) tracks employer resourcing rather than individual merit or interest?',
    'Longitudinal audit of RFC editorial teams, working-group chair affiliations, and requirements-discussion outcomes against participant employer size and meeting-attendance spend.',
    'High attribution raises effective extraction on payer seats and strengthens the encoded-gatekeeping account; low attribution would push the arrangement toward the stewardship reading''s low-extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(participation_control_attribution, empirical, 'Whether specification control tracks resourcing or merit.').

omega_variable(
    royalty_bearing_extension_prevalence,
    'What share of widely deployed nominally open standards carry royalty-bearing essential patent terms or proprietary extensions in practice, versus royalty-free commitments?',
    'Audit of IPR disclosures, licensing-declaration databases, and deployed-extension fragmentation studies across major protocol families.',
    'A high share converts published openness into a per-implementer toll and raises epsilon; a low share confines extraction to process-influence channels and softens the victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(royalty_bearing_extension_prevalence, empirical, 'Prevalence of royalty-bearing terms inside nominally open standards.').

omega_variable(
    fork_route_viability,
    'How viable is exit for payer seats — routing around captured standards via alternative bodies, open-source reimplementation, or de facto forks — and does that viability suppress measured extraction below what gatekeeping alone would produce?',
    'Comparative study of successful routing-around episodes (browser-spec fork defections, open-source reimplementation waves) versus failed ones, conditioned on the network-effect strength of the captured specification.',
    'High viability lowers effective suppression and caps extraction for mobile payer seats; low viability confirms trapped-target treatment and supports the hybrid-over-clean-coordination classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fork_route_viability, conceptual, 'Whether implementer exit routes are real or nominal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__capture_substrate_reading, 0, 38).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_capture_substrate_tr_t0, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(ietf_capture_substrate_tr_t0, observed).
narrative_ontology:measurement(ietf_capture_substrate_tr_t6, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement_basis(ietf_capture_substrate_tr_t6, observed).
narrative_ontology:measurement(ietf_capture_substrate_tr_t12, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement_basis(ietf_capture_substrate_tr_t12, observed).
narrative_ontology:measurement(ietf_capture_substrate_tr_t19, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 19, 0.35).
narrative_ontology:measurement_basis(ietf_capture_substrate_tr_t19, observed).
narrative_ontology:measurement(ietf_capture_substrate_tr_t25, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement_basis(ietf_capture_substrate_tr_t25, observed).
narrative_ontology:measurement(ietf_capture_substrate_tr_t31, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 31, 0.41).
narrative_ontology:measurement_basis(ietf_capture_substrate_tr_t31, observed).
narrative_ontology:measurement(ietf_capture_substrate_tr_t38, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 38, 0.42).
narrative_ontology:measurement_basis(ietf_capture_substrate_tr_t38, observed).

% Extraction over time
narrative_ontology:measurement(ietf_capture_substrate_be_t0, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(ietf_capture_substrate_be_t0, observed).
narrative_ontology:measurement(ietf_capture_substrate_be_t6, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 6, 0.34).
narrative_ontology:measurement_basis(ietf_capture_substrate_be_t6, observed).
narrative_ontology:measurement(ietf_capture_substrate_be_t12, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement_basis(ietf_capture_substrate_be_t12, observed).
narrative_ontology:measurement(ietf_capture_substrate_be_t19, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 19, 0.5).
narrative_ontology:measurement_basis(ietf_capture_substrate_be_t19, observed).
narrative_ontology:measurement(ietf_capture_substrate_be_t25, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement_basis(ietf_capture_substrate_be_t25, observed).
narrative_ontology:measurement(ietf_capture_substrate_be_t31, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 31, 0.57).
narrative_ontology:measurement_basis(ietf_capture_substrate_be_t31, observed).
narrative_ontology:measurement(ietf_capture_substrate_be_t38, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 38, 0.58).
narrative_ontology:measurement_basis(ietf_capture_substrate_be_t38, observed).

% Suppression requirement over time
narrative_ontology:measurement(ietf_capture_substrate_su_t0, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(ietf_capture_substrate_su_t0, observed).
narrative_ontology:measurement(ietf_capture_substrate_su_t6, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 6, 0.28).
narrative_ontology:measurement_basis(ietf_capture_substrate_su_t6, observed).
narrative_ontology:measurement(ietf_capture_substrate_su_t12, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement_basis(ietf_capture_substrate_su_t12, observed).
narrative_ontology:measurement(ietf_capture_substrate_su_t19, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 19, 0.38).
narrative_ontology:measurement_basis(ietf_capture_substrate_su_t19, observed).
narrative_ontology:measurement(ietf_capture_substrate_su_t25, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(ietf_capture_substrate_su_t25, observed).
narrative_ontology:measurement(ietf_capture_substrate_su_t31, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 31, 0.45).
narrative_ontology:measurement_basis(ietf_capture_substrate_su_t31, observed).
narrative_ontology:measurement(ietf_capture_substrate_su_t38, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 38, 0.47).
narrative_ontology:measurement_basis(ietf_capture_substrate_su_t38, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__capture_substrate_reading, global_infrastructure).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__legitimacy_erosion_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'IETF openness': the natural-language commitment covers three structurally distinct claims with different epsilon. This file instantiates the capture-substrate reading (moderate extraction; operators benefit, implementers and users pay). The commons-stewardship reading (ietf_openness_commitment__commons_stewardship_reading) treats the same process as public infrastructure preserving interoperability — low extraction, no victim set. The legitimacy-erosion reading (ietf_openness_commitment__legitimacy_erosion_reading) relocates the contest to the rough-consensus mechanism itself and its vulnerability to organized capture. The upstream stewardship claim is routinely cited as evidence the process is healthy, which is why this reading links to it: the family edges let contamination analysis test whether the stewardship profile survives contact with the participation-economics data this reading is built on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ietf_openness_commitment__capture_substrate_reading, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
