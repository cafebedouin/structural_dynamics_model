% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__commons_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__commons_stewardship_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: ietf_openness_commitment__commons_stewardship_reading
 *   human_readable: IETF Openness Commitment — Commons Stewardship Reading
 *   domain: technology governance / internet standards / institutional economics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   ietf_openness_commitment kernel: the commons_stewardship_reading, under
 *   which the IETF's openness commitment — open participation, freely
 *   implementable and royalty-free specifications, rough consensus and
 *   running code — operates as public-infrastructure coordination that
 *   preserves equal interoperability for all implementers, large and small.
 *   Per the epsilon-invariance discipline, the kernel contest is NOT
 *   described inside this constraint: the sibling readings
 *   (capture_substrate_reading, legitimacy_erosion_reading) are separate
 *   constraint stories with their own epsilon values, beneficiary/victim
 *   structures, and classifications, linked to this file through
 *   network.affects_constraints. The epsilon referent here is the standing
 *   arrangement — the operating IETF openness regime as it actually runs —
 *   assessed by this reading's own lights, never the idealized commons this
 *   reading would endorse. The claim/metric independence rule applies:
 *   claimed_type rope is stated from this reading's structural assessment;
 *   the metrics are authored as descriptive facts about the arrangement's
 *   actual operation, without tuning either to the other or to a predicted
 *   engine verdict. KEY AGENTS (by structural relationship): -
 *   ietf_leadership_iesg_iab: Agenda-setter (institutional/mobile) —
 *   administers the pipeline, interprets the openness norms -
 *   ietf_working_group_contributors: Participant-beneficiary with payer costs
 *   (organized/mobile) — donate specification labor -
 *   hyperscale_platform_vendors: Primary beneficiary (powerful/arbitrage) —
 *   deploy standards at fleet scale, toll-free - small_implementers_startups:
 *   Symmetric beneficiary (moderate/mobile) — interoperate without
 *   negotiating with incumbents - open_source_implementations: Beneficiary
 *   (organized/mobile) — reimplement specs freely in commons codebases -
 *   end_users_of_interoperable_internet: Passive beneficiary
 *   (powerless/constrained) — inherit interoperability -
 *   underrepresented_user_regions: Excluded voice (powerless/trapped) —
 *   affected by defaults, absent from the rooms -
 *   standards_governance_scholars: Analytical observer
 *   (analytical/analytical)
 *
 * KEY AGENTS:
 *   - ietf_leadership_iesg_iab — agenda_setter (institutional/mobile): runs the standards pipeline and interprets the openness norms when disputes arise
 *   - ietf_working_group_contributors — beneficiary/secondary payer (organized/mobile): donate specification labor, absorb participation costs, gain influence and interoperability
 *   - hyperscale_platform_vendors — beneficiary (powerful/arbitrage): deploy standardized protocols across enormous fleets without license tolls; retain proprietary alternatives
 *   - small_implementers_startups — beneficiary (moderate/mobile): read the same public specs as the largest vendors and ship interoperable products without permission or royalties
 *   - open_source_implementations — beneficiary (organized/mobile): reimplement published specs in freely redistributable codebases
 *   - end_users_of_interoperable_internet — beneficiary (powerless/constrained): use devices and networks that interoperate because the protocols are shared
 *   - underrepresented_user_regions — excluded (powerless/trapped): live inside protocol defaults they had no hand in setting
 *   - standards_governance_scholars — observer (analytical/analytical): study how the process allocates influence; hold no decision rights
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__commons_stewardship_reading, 0.16).
domain_priors:suppression_score(ietf_openness_commitment__commons_stewardship_reading, 0.1).
domain_priors:theater_ratio(ietf_openness_commitment__commons_stewardship_reading, 0.23).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 0.23).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__commons_stewardship_reading, rope).
narrative_ontology:human_readable(ietf_openness_commitment__commons_stewardship_reading, "IETF Openness Commitment — Commons Stewardship Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__commons_stewardship_reading, "technology governance / internet standards / institutional economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__commons_stewardship_reading, 'f258bec6-1bb9-458d-90bd-1306b5b5fca6').
narrative_ontology:cs_kernel_codification('f258bec6-1bb9-458d-90bd-1306b5b5fca6', formalized).
narrative_ontology:cs_authority_grounding('f258bec6-1bb9-458d-90bd-1306b5b5fca6', expertise).
narrative_ontology:cs_interpretation_layer_present('f258bec6-1bb9-458d-90bd-1306b5b5fca6').
narrative_ontology:cs_reading_relation('f258bec6-1bb9-458d-90bd-1306b5b5fca6', ietf_openness_commitment__capture_substrate_reading, coexists_with).
narrative_ontology:cs_reading_relation('f258bec6-1bb9-458d-90bd-1306b5b5fca6', ietf_openness_commitment__legitimacy_erosion_reading, coexists_with).
narrative_ontology:cs_axiom('f258bec6-1bb9-458d-90bd-1306b5b5fca6', foundational, equal_interoperability_entitlement).
narrative_ontology:cs_axiom_status(equal_interoperability_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('f258bec6-1bb9-458d-90bd-1306b5b5fca6', equal_interoperability_entitlement, deontological).
narrative_ontology:cs_axiom('f258bec6-1bb9-458d-90bd-1306b5b5fca6', secondary, openness_neutralizes_resource_asymmetry).
narrative_ontology:cs_axiom_status(openness_neutralizes_resource_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('f258bec6-1bb9-458d-90bd-1306b5b5fca6', openness_neutralizes_resource_asymmetry, empirically_contingent).
narrative_ontology:cs_reference_frame('f258bec6-1bb9-458d-90bd-1306b5b5fca6', interoperability_commons_charter).
narrative_ontology:cs_drift_state('f258bec6-1bb9-458d-90bd-1306b5b5fca6', contemporary_scale_pressure_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('f258bec6-1bb9-458d-90bd-1306b5b5fca6', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__commons_stewardship_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, ietf_working_group_contributors).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, hyperscale_platform_vendors).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, small_implementers_startups).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, open_source_implementations).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, end_users_of_interoperable_internet).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ietf_openness_commitment__commons_stewardship_reading, ietf_working_group_contributors).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__commons_stewardship_reading, rough_consensus_running_code_doctrine).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__commons_stewardship_reading, open_standards_public_good_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Area directors, working-group chairs, and the Internet Architecture Board run the standards pipeline: they admit working groups, approve documents as RFCs, interpret the openness and intellectual-property rules when disputes arise, and hear appeals. They serve as volunteers alongside technical day jobs and rotate out; the institution outlasts any individual, and stepping down carries no penalty.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, ietf_leadership_iesg_iab, agenda_setter,
    institutional, generational, mobile, global).

% Engineers, researchers, and hobbyists who draft, debate, and review specifications. They donate employer-funded or personal time, absorb travel and meeting costs, and accept compromises that make designs worse for their own products in exchange for specifications everyone can build on. Leaving costs them influence, not access — the published RFCs remain theirs to implement.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, ietf_working_group_contributors, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__commons_stewardship_reading, ietf_working_group_contributors, payer).

% Large cloud, operating-system, and platform companies deploy standardized protocols across enormous fleets. They take the specifications without license fees, and their internal alternatives — proprietary overlays and captive ecosystems — remain available if a standard stops serving them, so their continued participation is a running choice rather than a captivity. They also fund a large share of the specification labor.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, hyperscale_platform_vendors, beneficiary,
    powerful, generational, arbitrage, global).

% Small firms and new entrants read the same public specifications as the largest vendors and ship interoperable products without negotiating permission or paying royalties. Their alternative — building a proprietary stack and seeking partners one contract at a time — is expensive but lawful, so taking the open route is a competitive calculation rather than a forced move.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, small_implementers_startups, beneficiary,
    moderate, biographical, mobile, global).

% Volunteer- and foundation-maintained codebases — kernel networking stacks, TLS libraries, routing daemons — reimplement the published specifications and redistribute them freely. Nothing binds them to the process; they consume its outputs and occasionally feed implementation experience and patches back.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, open_source_implementations, beneficiary,
    organized, generational, mobile, global).

% Billions of people use devices, networks, and applications that interoperate because the underlying protocols are shared. They never see the process, cannot opt out of the protocol suite, and pay nothing for it directly; their exposure runs entirely through whatever equipment and services they buy.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, end_users_of_interoperable_internet, beneficiary,
    powerless, biographical, constrained, global).

% Communities in low-bandwidth, high-latency, or low-income regions live inside protocol defaults they had no hand in setting — page-weight assumptions, handshake counts, an English-language process culture, meeting hours set for Northern-hemisphere business days. Liaison channels exist but are rarely used; their objections tend to surface after deployment, as performance complaints rather than design input.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, underrepresented_user_regions, excluded,
    powerless, biographical, trapped, global).

% Academic researchers in institutional economics and science-and-technology studies who attend meetings, interview participants, and publish analyses of how the process allocates influence. They hold no decision rights and bear none of the costs; their output is the public record the other seats argue with.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, standards_governance_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__commons_stewardship_reading, diffuse).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__commons_stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the fragmentation problem: without a common open specification process, each vendor builds incompatible private protocols and networks become islands. The openness commitment coordinates competing implementers onto shared, freely implementable specifications so heterogeneous systems interoperate, and it does so while leaving proprietary alternatives lawfully available to anyone who prefers them.
% TRANSFER_FUNCTION: Moves contributed engineering expertise and review attention from participant organizations into a public specification corpus, and distributes interoperability rights back out to all implementers without payment. It concurrently blocks the reverse flow — no participant can convert adopted-specification control into license revenue.
% ABSENT_VOICES: Populations affected by protocol decisions but absent from the working groups: non-technical end users, low-bandwidth and low-income region communities, and non-Western operator cultures. They would object to bandwidth-heavy defaults, English-only process culture, and meeting hours tuned to Northern-hemisphere business days. They sit outside the participant pool, reachable only through liaison channels almost nobody uses; their interests enter mostly as post-deployment performance complaints.
% DISAPPEARANCE_RATIONALE: If the openness commitment vanished overnight, the multi-vendor interoperability fabric would rearrange: basic protocols would acquire licensing tolls or fork into incompatible vendor variants, the open-source networking stack would lose its legal and practical footing, small implementers would need bilateral agreements to interoperate at all, and the network would stratify into walled gardens along existing market-power lines within years.
% FOUNDING_PROBLEM: Early network protocol proliferation — vendor-specific stacks (SNA, DECnet, IPX) and ad hoc research protocols — produced islands of incompatible networks; the arrangement was built to let heterogeneous networks interoperate without ceding control of the shared specifications to any single vendor or government.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: telecommunications regulators cite fragmentation harms in impact assessments accompanying mandated-interoperability legislation; academic histories of the pre-TCP/IP era document the vendor-stack incompatibility the process was built against; and the recurrence of fragmentation pressure at each new layer (messaging silos, IoT naming, AI interconnects) is attested in industry filings by parties with no stake in the IETF's reputation.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__commons_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__commons_stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__commons_stewardship_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ietf_openness_commitment__commons_stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__commons_stewardship_reading, 0.16, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__commons_stewardship_reading_tests).
:- end_tests(ietf_openness_commitment__commons_stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.16 at interval end) because no seat collects a transfer from another: the residual above the information_standard floor (0.02) reflects real but bounded asymmetries — participation burden, compromise designs that fit no one optimally, and mild scale advantages in shaping drafts. Suppression is authored very low (0.10) and is a raw structural property, unscaled by scope: exits are wide open (fork the spec, build a proprietary stack, take the work to another SDO), and the arrangement recruits by usefulness rather than coercion. Theater ratio (0.23) captures accumulated process ritual — boilerplate, last-call formalities, honorific observance of process steps — while the core function of producing implementable specifications remains dominant. Accessibility collapse (0.42) sits where a rope should: once an organization commits to multi-vendor interoperability, proprietary-silo alternatives stop serving that goal, but silo strategies remain genuinely available and are sometimes chosen outside the standard's ambit. Resistance (0.38) is moderate and persistent: recurring attempts to steer specifications toward incumbent implementations, RAND-preferring patent positions, and embrace-and-extend maneuvers meet process defenses and are blunted rather than eliminated. The measurement series run on one shared time grid (six points, both metrics authored at every point) so no metric row borrows another's endpoints.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is structural, not rhetorical. From the agenda-setter seat the arrangement is a treasured commons its stewards maintain; from the hyperscale vendor seat it is free infrastructure they would otherwise pay tolls for — and occasionally attempt to bend; from the small-implementer seat it is the only reason they can compete at all; from the contributor seat it is influence purchased with donated labor. One further seat is deliberately NOT seated as a victim here: a RAND-preferring patent holder experiences the same openness norm as expropriation of licensable property, but under this reading they bear no extraction — they forgo rents the norm refuses to let them collect. The engine computes per-seat classifications from the structural data; the sibling files author those divergent perceptions as separate constraints rather than averaging them here.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared beneficiary derives a low directionality value, so effective extraction is damped toward or below the coordination floor for all of them. Contributors carry mild payer costs through their secondary role (donated labor, travel, compromise), pulling their directionality slightly above the pure-beneficiary end while remaining far from the target end. No victims are declared because, under this reading, no agent bears asymmetric extraction: the beneficiary set is coextensive with the governed population, which is the signature of pure coordination rather than a privileged class. End users combine a passive beneficiary position with trapped exit; trapped exit amplifies effective extraction only when extraction exists, and with the beneficiary declaration dominant their directionality stays at the subsidized end. No directionality overrides are authored: the derivation from beneficiary declarations and exit options already produces the correct qualitative profile for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: the founding problem — fragmentation of heterogeneous networks into incompatible vendor islands — is still live, recurring at every new layer (IoT naming, messaging interconnection, AI-system interconnects), so no mandate has outlived its function and no piton dynamics are asserted. The classification disciplines mislabeling in both directions: reading this rope as a snare would demand dismantling load-bearing infrastructure whose removal cost is prohibitive relative to any benefit (see fixing_cost); reading it as a mountain would immunize it from exactly the scrutiny the sibling readings author — the openness commitment is a maintained human arrangement, not a natural law, and it earns its low extractiveness continuously rather than possessing it. The R5 mismatch consumer finds status=live paired with verdict=world_rearranges, which flags nothing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'Does the standing IETF openness arrangement exhibit the low-extraction, symmetric-constraint profile this reading (commons_stewardship_reading of kernel ietf_openness_commitment) authors, or the resource-gated profile the capture_substrate_reading sibling authors?',
    'Participation-influence audit: correlate organizational resourcing (engineer attendance volume, draft authorship share, chair and leadership positions held) with editorial control over adopted specifications; if influence tracks resourcing beyond merit proxies, the capture reading''s structure fits the standing arrangement better.',
    'If influence tracks resourcing, effective extraction rises for unresourced implementers and this story''s classification migrates rope toward tangled_rope; the sibling file''s beneficiary/victim structure becomes the better fit and this reading''s foundational axiom openness_neutralizes_resource_asymmetry loses empirical support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, empirical, 'Which sibling reading''s structure the standing arrangement actually matches; locates the kernel contest at the resource-asymmetry element.').

omega_variable(
    participation_cost_extraction_status,
    'Are rising participation costs (meeting travel, engineer time, growing document complexity) a burden borne disproportionately by small implementers — a regressive component of epsilon — or ordinary coordination cost spread by voluntary choice?',
    'Compare specification-adoption rates and implementation lag for small versus heavily resourced implementers against participation-cost trends; remote and hybrid meeting participation after 2020 supplies a natural experiment on the cost channel.',
    'If small implementers'' adoption lags track cost growth, part of the measured extractiveness is regressive and the symmetric-constraint claim weakens; if adoption is uniform across resourcing levels, the costs are benign coordination overhead near the information_standard floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(participation_cost_extraction_status, empirical, 'Whether the residual extractiveness this reading authors is distributionally neutral or regressive.').

omega_variable(
    consensus_mechanism_capture_exposure,
    'Is rough consensus decision-making robust to organized interest, or vulnerable to organized capture as the legitimacy_erosion_reading sibling contends?',
    'Historical audit of consensus outcomes: identify decisions where organized blocs moved results against documented technical objection, count frequency, and check reversibility through the appeals process.',
    'Frequent unreversed bloc wins would erode the legitimacy of the coordination function itself, raising measured suppression and pushing classification toward tangled_rope even under this reading''s own lights.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consensus_mechanism_capture_exposure, empirical, 'Robustness of the decision procedure that operationalizes the openness commitment.').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel best framed as the codified openness rules themselves (BCPs, IPR policy, process documents), or as the legitimacy narrative of neutral open process layered above those rules?',
    'Examine disputed process rulings: do outcomes track the written rules or the neutrality narrative when the two diverge (appeals upheld on rule grounds versus reputation-preservation grounds)?',
    'If the narrative is the operative kernel, authority_grounding shifts toward a practice/extraction hybrid and interpretation_layer_present changes meaning; the declared formalized-kernel framing would understate how much drift the narrative absorbs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative framings of the commitment-system kernel beneath this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__commons_stewardship_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t1992, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 1992, 0.1).
narrative_ontology:measurement_basis(ietf_tr_t1992, observed).
narrative_ontology:measurement(ietf_tr_t1999, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 1999, 0.13).
narrative_ontology:measurement_basis(ietf_tr_t1999, observed).
narrative_ontology:measurement(ietf_tr_t2006, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 2006, 0.17).
narrative_ontology:measurement_basis(ietf_tr_t2006, observed).
narrative_ontology:measurement(ietf_tr_t2013, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 2013, 0.19).
narrative_ontology:measurement_basis(ietf_tr_t2013, observed).
narrative_ontology:measurement(ietf_tr_t2018, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 2018, 0.21).
narrative_ontology:measurement_basis(ietf_tr_t2018, observed).
narrative_ontology:measurement(ietf_tr_t2024, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 2024, 0.23).
narrative_ontology:measurement_basis(ietf_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(ietf_be_t1992, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 1992, 0.09).
narrative_ontology:measurement_basis(ietf_be_t1992, observed).
narrative_ontology:measurement(ietf_be_t1999, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 1999, 0.11).
narrative_ontology:measurement_basis(ietf_be_t1999, observed).
narrative_ontology:measurement(ietf_be_t2006, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 2006, 0.13).
narrative_ontology:measurement_basis(ietf_be_t2006, observed).
narrative_ontology:measurement(ietf_be_t2013, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 2013, 0.14).
narrative_ontology:measurement_basis(ietf_be_t2013, observed).
narrative_ontology:measurement(ietf_be_t2018, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 2018, 0.15).
narrative_ontology:measurement_basis(ietf_be_t2018, observed).
narrative_ontology:measurement(ietf_be_t2024, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 2024, 0.16).
narrative_ontology:measurement_basis(ietf_be_t2024, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ietf_openness_commitment__commons_stewardship_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__commons_stewardship_reading, information_standard).
narrative_ontology:affects_constraint(ietf_openness_commitment__commons_stewardship_reading, ietf_openness_commitment__capture_substrate_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__commons_stewardship_reading, ietf_openness_commitment__legitimacy_erosion_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one colloquial concept — 'IETF openness' — decomposes into three structurally distinct claims per the epsilon-invariance principle. This file authors the commons-stewardship claim (low epsilon, symmetric constraint, no beneficiary class). The capture_substrate_reading authors a high-extraction variant with identifiable gatekeeping beneficiaries; the legitimacy_erosion_reading authors a contested-decision-procedure variant centered on consensus robustness. Each member carries its own epsilon, stakeholders, and claimed type; the edges here assert family membership and mutual relevance, not agreement. Upstream/downstream: the commons reading is the higher-confidence baseline the other two define themselves against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
