% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__legitimacy_erosion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__legitimacy_erosion_reading, []).

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
 *   constraint_id: ietf_openness_commitment__legitimacy_erosion_reading
 *   human_readable: IETF Rough Consensus Mechanism — Legitimacy-Erosion Reading
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates the legitimacy-erosion reading of the
 *   ietf_openness_commitment kernel: the IETF's rough consensus process
 *   ('rough consensus and running code') treated as a standing arrangement
 *   whose procedural legitimacy is itself the contested resource. On this
 *   reading the process genuinely produces interoperable protocol
 *   specifications — the coordination function is real — but its output
 *   legitimacy is manufactured by procedure and harvested by whichever
 *   factions can sustain organized participation: large vendors and
 *   hyperscale operators field permanent delegations, hold editorships and
 *   chairships, outlast dissent in years-long threads, and present deployed
 *   scale as accomplished consensus. Each ratified outcome transfers decision
 *   weight to the sponsoring faction and spends a portion of the process's
 *   accumulated credibility; the standing victim is the consensus mechanism's
 *   credibility commons, borne concretely by unorganized contributors whose
 *   objections are recorded and overridden, by small implementers who inherit
 *   vendor-shaped specifications, and by end users who receive the resulting
 *   protocols. This file is one member of a three-story constraint family
 *   decomposing the colloquial label 'IETF openness'; the siblings
 *   instantiate the commons-stewardship and capture-substrate readings with
 *   their own epsilon values and victim structures (see kernel_context and
 *   network.dual_formulation_note). KEY AGENTS (by structural relationship):
 *   - well_resourced_vendor_delegations: Primary beneficiary
 *   (institutional/arbitrage) — converts procedural presence into ratified
 *   architecture - dominant_platform_operators: Secondary beneficiary
 *   (institutional/arbitrage) — deployment scale reads as consensus -
 *   independent_contributor_participants: Primary target
 *   (moderate/identity_locked) — supplies review labor, loses outcome control
 *   - small_scale_implementers: Secondary target (moderate/constrained) —
 *   bears compliance costs of skewed specifications -
 *   ietf_process_administrators: Agenda setter (institutional/constrained) —
 *   administers safeguards, bears reputational risk - protocol_end_users:
 *   Absent affected population (powerless/trapped) — inherits design choices,
 *   no seat - civil_society_participants: Outweighed participant
 *   (organized/constrained) - internet_governance_researchers: Analytical
 *   observer (analytical/analytical) — sees full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, 0.71).
domain_priors:suppression_score(ietf_openness_commitment__legitimacy_erosion_reading, 0.48).
domain_priors:theater_ratio(ietf_openness_commitment__legitimacy_erosion_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__legitimacy_erosion_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__legitimacy_erosion_reading, "IETF Rough Consensus Mechanism — Legitimacy-Erosion Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__legitimacy_erosion_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__legitimacy_erosion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__legitimacy_erosion_reading, 'ed2826e5-466f-4c4d-ae14-5dd0072d2ea1').
narrative_ontology:cs_kernel_codification('ed2826e5-466f-4c4d-ae14-5dd0072d2ea1', formalized).
narrative_ontology:cs_authority_grounding('ed2826e5-466f-4c4d-ae14-5dd0072d2ea1', practice).
narrative_ontology:cs_interpretation_layer_present('ed2826e5-466f-4c4d-ae14-5dd0072d2ea1').
narrative_ontology:cs_reading_relation('ed2826e5-466f-4c4d-ae14-5dd0072d2ea1', ietf_openness_commitment__commons_stewardship_reading, influences).
narrative_ontology:cs_reading_relation('ed2826e5-466f-4c4d-ae14-5dd0072d2ea1', ietf_openness_commitment__capture_substrate_reading, coexists_with).
narrative_ontology:cs_axiom('ed2826e5-466f-4c4d-ae14-5dd0072d2ea1', foundational, procedural_openness_insufficient_for_legitimacy).
narrative_ontology:cs_axiom_status(procedural_openness_insufficient_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('ed2826e5-466f-4c4d-ae14-5dd0072d2ea1', procedural_openness_insufficient_for_legitimacy, instrumental).
narrative_ontology:cs_axiom('ed2826e5-466f-4c4d-ae14-5dd0072d2ea1', secondary, ratification_without_balance_spends_credibility).
narrative_ontology:cs_axiom_status(ratification_without_balance_spends_credibility, holdable).
narrative_ontology:cs_axiom_grounding('ed2826e5-466f-4c4d-ae14-5dd0072d2ea1', ratification_without_balance_spends_credibility, empirically_contingent).
narrative_ontology:cs_reference_frame('ed2826e5-466f-4c4d-ae14-5dd0072d2ea1', open_participation_legitimacy_pact).
narrative_ontology:cs_drift_state('ed2826e5-466f-4c4d-ae14-5dd0072d2ea1', contemporary_hyperscale_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ed2826e5-466f-4c4d-ae14-5dd0072d2ea1', '2026-06-14T09:30:00Z').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_vendor_delegations).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, dominant_platform_operators).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, independent_contributor_participants).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, small_scale_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, protocol_end_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, civil_society_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Send permanent teams of protocol engineers to the working groups touching their product lines, hold editorships and chairships in the groups that matter most, sustain participation through years-long email threads where shorter-lived voices drop out, and arrive at consensus calls with deployed implementations that present their preferred designs as accomplished facts. When a working group outcome goes against them they can redirect effort to industry consortia or deploy at scale and let usage settle the question, so their presence is a continuing investment decision rather than a commitment.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_vendor_delegations, beneficiary,
    institutional, generational, arbitrage, global).

% Operate networks large enough that their deployment choices read as market consensus; benefit when ratified protocols encode traffic volumes, latency budgets, and operational assumptions that only planet-scale operators can satisfy. Fund attendance and specification work disproportionately, and can shift standardization energy to captive venues if the open process turns unfavorable.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, dominant_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Individual engineers, researchers, and experienced hobbyists who contribute design review, security analysis, and implementation feedback without organizational backing. Their objections are recorded in threads and polls but are routinely outlasted by organized delegations that keep arguing after individuals return to day jobs; consensus calls then close over their recorded dissent. Many have built careers and reputations around participation, so stepping back means leaving the community where their expertise is recognized, not merely forfeiting a vote.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, independent_contributor_participants, payer,
    moderate, biographical, identity_locked, global).

% Small vendors, startups, and open-source projects that must ship code compatible with whatever the process ratifies. Specifications weighted toward large-operator requirements raise their implementation and testing costs, and once a ratified baseline reaches deployed software, building around it is not realistic; their leverage is limited to comments during development windows.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, small_scale_implementers, payer,
    moderate, biographical, constrained, global).

% Working group chairs, area directors, and the appeals bodies who run consensus calls, weigh humming, judge whether objection is substantive or disruptive, appoint editors, and adjudicate appeals. They operate the conflict-of-interest rules and participation guidelines meant to keep organized factions from overwhelming individual voices, and they bear the reputational damage when outcomes look prearranged — while depending on the same large delegations for much of the technical labor the process runs on. Resignation is possible; abandoning the process would mean leaving the institution their professional standing rests on.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, ietf_process_administrators, agenda_setter,
    institutional, generational, constrained, global).

% The billions of people whose connections run over ratified protocols. They experience the process's output choices as privacy defaults, performance characteristics, and the entrenchment or displacement of incumbent services, but no working group seat represents them; they learn of consequential design decisions after deployment, when switching costs are already sunk.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, protocol_end_users, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__legitimacy_erosion_reading, protocol_end_users, excluded).

% Privacy advocates, digital-rights organizations, and public-interest technologists who attend the working groups where surveillance-relevant and architecture-defining choices are made. They are consistently outnumbered by vendor delegations, funded to attend far fewer meetings than their opponents, and their interventions shape documents at the margins while core architecture follows resourced participants' priorities.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, civil_society_participants, payer,
    organized, biographical, constrained, global).

% Academic and policy researchers who study standards-body governance, document participation concentration and outcome alignment, and publish analyses of legitimacy dynamics in internet standardization. They observe the full structure, collect no benefit from the process, and bear none of its operating costs.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, internet_governance_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_vendor_delegations).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__legitimacy_erosion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of producing interoperable protocol specifications among thousands of voluntarily participating, mutually distrusting implementers without any authority empowered to command them: pooling scarce protocol-design expertise, testing proposals against running code, and converting sprawling technical argument into single ratified documents that everyone implements.
% TRANSFER_FUNCTION: Moves decision weight and ratified-outcome control from unorganized individual contributors to whichever factions can sustain organized participation; moves attention, review labor, and deference from participants into the process; and moves the process's accumulated credibility onto each ratified document, including documents that encode the sponsoring faction's commercial architecture.
% ABSENT_VOICES: Protocol end users would object to vendor-weighted design choices but have no seat in any working group; non-participating implementers in less-connected markets inherit decisions they never saw; engineers without employer travel budgets or spare hours are silent in the threads where outcomes form; future users are represented by no one. Civil society participants attend but are chronically outnumbered — present in the room, absent from its arithmetic.
% DISAPPEARANCE_RATIONALE: If the rough consensus process vanished overnight, protocol development would reorganize within months around vendor consortia and deployment-driven de facto standardization dominated by the largest operators; interoperability would fragment into walled-garden compatibility zones; small implementers and independent contributors would lose their only accessible venue; and the largest operators would hold more control than the captured process currently cedes them — the rearrangement demonstrates that present arrangements depend on the mechanism existing, even in its eroded state.
% FOUNDING_PROBLEM: In the early ARPANET and Internet era, protocol design was ad hoc and host-vendor-specific; the founding problem was getting competing research groups and vendors to agree on common wire formats and behaviors without any authority empowered to command agreement — hence 'rough consensus and running code': accept imperfect agreement backed by demonstrated implementation rather than waiting for unanimity or installing a hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: Internet historians and early participants' memoirs — sources outside today's benefiting factions — attest that hierarchy-free coordination was a real unsolved problem and that the founding solution fit its era. Institutional economists studying standards bodies corroborate that the underlying problem persists but in mutated form: participant scale asymmetry now dominates the coordination landscape in ways the founding-era accounts never contemplated. No corroboration of the founding narrative comes from the well-resourced delegations themselves, whose attestation doubles as justification for their privileged position, and the parties dispute whether the founding problem survives in recognizable form at all.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__legitimacy_erosion_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__legitimacy_erosion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ietf_openness_commitment__legitimacy_erosion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__legitimacy_erosion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ietf_openness_commitment__legitimacy_erosion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) because the currency extracted is legitimacy and ratified-outcome control rather than fees: the process converts voluntary contribution and deference into binding documents, and organized factions steer which documents bind and what they contain, so the value transferred scales with the process's credibility rather than with any participant's payment. Suppression is moderate (0.48) and is authored as a raw structural property, unscaled by power or scope: there is no coercion of persons, but procedural gatekeeping — chair discretion over consensus calls, the practical difficulty of successful appeals, the attrition of unorganized voices in extended threads — raises the cost of dissent without eliminating it. Theater ratio 0.42 reflects the growing share of process activity that performs consensus rather than tests it: ritual hums, pre-negotiated outcomes presented for acclamation, last calls that ratify rather than solicit. Accessibility collapse 0.52: alternatives exist (consortia, de facto deployment standardization, rival bodies) but are fragmented and lack the process's convening legitimacy, so understanding the mechanism does not dissolve dependence on it. Resistance 0.58: appeals, public dissent statements, splinter efforts, and a persistent governance-critique literature constitute real ongoing resistance. Claim and metrics are independent authored facts: tangled_rope is claimed from structure — a genuine specification-production function fused with asymmetric extraction of legitimacy, held together by active procedural enforcement — while the metric values describe observed operation. The measurement series run on one shared seven-point grid so every tracked metric is authored at every examined time point; the rising suppression_requirement series traces the hardening of safeguard machinery (conflict-of-interest rules, consensus-call guidance, appeals formalization) in response to growing capture pressure, not growing coercion of persons.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the resourced-delegation seat the process is a fair exchange won by out-investing rivals: participation costs are recouped in ratified architecture, and the safeguard apparatus is overhead to be managed. From the independent-contributor seat the same process operates as a machine that records objections and overrules them, charging years of unpaid labor for the privilege of losing. From the administrator seat it is a defensive perimeter requiring constant vigilance — chairs experience capture pressure as workload and reputational risk, not as benefit. The engine computes per-seat classifications from the structural data; the divergence between the beneficiary seats' coordination experience and the payer seats' extraction experience is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive well_resourced_vendor_delegations and dominant_platform_operators toward the subsidy pole: ratified outcomes subsidize their product architectures, and their arbitrage-grade exits (consortia, de facto deployment at scale) mean the process needs them more than they need it. Victim declarations drive independent_contributor_participants and small_scale_implementers toward the full-target pole: they supply the review labor and bear the compliance costs while organized blocs harvest the outcomes; the contributors' identity lock holds them nearer the trapped end than their nominal mobility suggests. protocol_end_users occupy the extreme target position on privacy-default and entrenchment costs but so diffusely, and from so far outside the room, that their contribution is attenuated by absence. ietf_process_administrators sit near symmetric: they neither collect the harvested legitimacy nor primarily bear it — they expend effort defending the arrangement, which nets to near-zero flow. civil_society_participants are moderately targeted: present in the conversation but persistently outweighted in it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating mutually distrusting peers without hierarchy — remains live in mutated form, so this is not a resolved-mandatrophy case: the arrangement has not outlived its function; it has grown a second function, legitimacy manufacture, that partially displaces the first. The tangled_rope classification prevents mislabeling in both directions: reading the process as pure extraction would erase the real specification-production function that still delivers interoperability no rival venue provides; reading it as pure coordination would erase the documented asymmetry between organized and unorganized participants. The inertial-theatrical failure mode is live as a risk rather than a present state: if credibility depletion crosses the compounding threshold (omega credibility_threshold_reversibility), the coordination function atrophies into ritual while the apparatus persists — the theater_ratio series is the early-warning instrument for that transition, and its steady climb from 0.22 to 0.42 is the quantitative trace of the mandate slowly separating from the function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates one reading (legitimacy_erosion) of the ietf_openness_commitment kernel; what structural facts would change if a sibling reading were adopted instead?',
    'Compare the three family stories'' epsilon, beneficiary, and victim structures side by side; the disagreement is located in where the extraction sits (legitimacy flow versus encoded outputs versus nowhere) and who the standing victim is.',
    'Under commons_stewardship_reading the same process computes with low epsilon and broad implementer-wide beneficiaries; under capture_substrate_reading extraction relocates to ratified specifications and small implementers become the primary victims. Classification of the shared kernel is reading-indexed, not topic-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed epsilon over a shared kernel; sibling readings relocate extraction and victims.').

omega_variable(
    legitimacy_extraction_measurement,
    'How is extraction measured when the currency is procedural legitimacy and ratified-outcome control rather than money?',
    'Outcome-distribution studies: authorship and editorship concentration, survival rates of proposals by sponsoring faction, appeal success rates by filer class, and attendance concentration data across working groups.',
    'If outcome alignment tracks sponsor resources, the high epsilon stands; if outcomes track technical merit independent of faction resources, epsilon falls toward the coordination-cost range and the reading weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_extraction_measurement, empirical, 'Measurability of legitimacy-denominated extraction.').

omega_variable(
    merit_vs_resource_confound,
    'Are faction-aligned outcomes captured, or do resourced participants simply field more competent experts whose designs win on quality?',
    'Natural experiments where resourced factions lost despite resource superiority; regression of outcome alignment on resource concentration controlling for measured proposal quality.',
    'If merit explains outcomes, the extraction reading overstates capture and the arrangement sits nearer pure coordination; if resources predict outcomes controlling for quality, organized capture is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(merit_vs_resource_confound, empirical, 'Competence advantage versus organized capture as the driver of faction-aligned outcomes.').

omega_variable(
    contributor_identity_lock_mechanism,
    'Is independent contributors'' persistence under adverse outcomes structural dependence on the only accessible venue, or internalized identity fusion with the participant role?',
    'Post-exit trajectory studies of departed contributors: whether they report equivalent influence and recognition elsewhere, or loss of professional meaning that kept them contributing through losing streaks.',
    'If identity-locked, effective suppression exceeds the structural measure and exit-based remedies fail; if structural, credible venue pluralism would rebalance participation without individual defection being required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contributor_identity_lock_mechanism, empirical, 'Structural versus internalized retention of unorganized contributors.').

omega_variable(
    credibility_threshold_reversibility,
    'Has legitimacy erosion passed a compounding threshold where each procedurally valid but faction-ratified outcome cheapens the next, or does the process retain self-correction capacity?',
    'Longitudinal series on appeal volume and success, splinter-exodus events, and adoption share of ratified specifications versus rival consortium or de facto standards.',
    'Past the threshold, the arrangement drifts toward inertial-theatrical persistence with the coordination function atrophying; before it, the coordination function remains recoverable and the hybrid classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credibility_threshold_reversibility, empirical, 'Whether credibility depletion compounds or self-corrects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__legitimacy_erosion_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ietf_tr_t6, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(ietf_tr_t12, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(ietf_tr_t18, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 18, 0.33).
narrative_ontology:measurement(ietf_tr_t24, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(ietf_tr_t30, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(ietf_tr_t36, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 36, 0.42).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ietf_be_t6, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(ietf_be_t12, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(ietf_be_t18, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 18, 0.57).
narrative_ontology:measurement(ietf_be_t24, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(ietf_be_t30, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(ietf_be_t36, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 36, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ietf_su_t6, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 6, 0.3).
narrative_ontology:measurement(ietf_su_t12, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(ietf_su_t18, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 18, 0.39).
narrative_ontology:measurement(ietf_su_t24, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 24, 0.43).
narrative_ontology:measurement(ietf_su_t30, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement(ietf_su_t36, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 36, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__legitimacy_erosion_reading, information_standard).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__capture_substrate_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'IETF openness' conflates three structurally distinct claims, decomposed per the epsilon-invariance principle: (1) that the open process stewards public interoperability infrastructure (commons_stewardship_reading — low extraction, beneficiaries are all implementers); (2) that the process is a substrate where resource advantage encodes itself into gatekeeping (capture_substrate_reading — extraction located in ratified specifications); (3) that the consensus mechanism's legitimacy is itself the contested, depletable resource (this story — extraction located in the legitimacy flow, victim is the credibility commons). Each carries its own epsilon, beneficiaries, and victims; the family is linked via affects_constraints. Direction: the stewardship reading is the established claim whose credibility the resourced factions invoke as cover, so this erosion reading's diagnosis structurally pressures the stewardship claim (undermining its neutrality warrant) while coexisting with the substrate reading as a parallel diagnosis held by overlapping communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
