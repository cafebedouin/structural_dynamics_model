% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__capture_substrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Internet Standards Process as Coordination Substrate with Encoded Gatekeeping (Capture Substrate Reading)
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   Under this reading, the Internet standards process is a coordination
 *   substrate that genuinely produces interoperable specifications — and
 *   whose internal structure converts resource advantage into encoded
 *   gatekeeping. Large platform operators sustain standing delegations,
 *   author a plurality of drafts, hold procedural offices, and deploy at
 *   scales that make their implementations the reference behavior; the
 *   specifications that emerge carry their architectural commitments, and the
 *   extension and licensing surfaces they prefer raise rivals' compliance
 *   costs. Small implementers and end users receive real interoperability
 *   while bearing the asymmetries: specification complexity sized to large
 *   operators' needs, licensing terms negotiated bilaterally by
 *   essential-patent holders, and lock-in to ecosystem boundaries drawn in
 *   rooms they could not enter. The constraint claimed here is the hybrid: a
 *   functioning coordination machine whose output distribution is steered by
 *   input asymmetry and actively defended by the participants it favors. KEY
 *   AGENTS (by structural relationship): - hyperscale_platform_operators:
 *   Effective agenda-setter and primary beneficiary (institutional/arbitrage)
 *   - essential_patent_holders: Secondary beneficiary (powerful/mobile) -
 *   small_implementers: Primary target (moderate/constrained) -
 *   independent_software_vendors: Target (moderate/constrained) - end_users:
 *   Diffuse target and incidental beneficiary (powerless/trapped) -
 *   ietf_leadership_iesg: Formal agenda-setter
 *   (institutional/identity_locked) - public_interest_advocates: Excluded
 *   voice (powerless/trapped) - competition_authorities: Analytical observer
 *   (institutional/analytical)
 *
 * KEY AGENTS:
 *   - hyperscale_platform_operators: Effective agenda-setter and primary beneficiary (institutional/arbitrage) — converts deployment scale and delegation capacity into specification content
 *   - essential_patent_holders: Secondary beneficiary (powerful/mobile) — collects licensing revenue from every compliant implementer
 *   - small_implementers: Primary target (moderate/constrained) — implements the full specification surface without agenda access
 *   - independent_software_vendors: Target (moderate/constrained) — pays royalties and compliance costs to reach the market the standard defines
 *   - end_users: Diffuse target and incidental beneficiary (powerless/trapped) — receives interoperability, absorbs lock-in
 *   - ietf_leadership_iesg: Formal agenda-setter (institutional/identity_locked) — administers the process and ratifies outcomes pre-shaped by resource-rich participants
 *   - public_interest_advocates: Excluded voice (powerless/trapped) — priced out of sustained participation
 *   - competition_authorities: Analytical observer (institutional/analytical) — investigates licensing conduct and process dominance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, 0.58).
domain_priors:suppression_score(ietf_openness_commitment__capture_substrate_reading, 0.46).
domain_priors:theater_ratio(ietf_openness_commitment__capture_substrate_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0.46).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__capture_substrate_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__capture_substrate_reading, "Internet Standards Process as Coordination Substrate with Encoded Gatekeeping (Capture Substrate Reading)").
narrative_ontology:topic_domain(ietf_openness_commitment__capture_substrate_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__capture_substrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__capture_substrate_reading, 'c5e28e28-5652-497d-aea8-3e611e9a077b').
narrative_ontology:cs_kernel_codification('c5e28e28-5652-497d-aea8-3e611e9a077b', formalized).
narrative_ontology:cs_authority_grounding('c5e28e28-5652-497d-aea8-3e611e9a077b', practice).
narrative_ontology:cs_interpretation_layer_present('c5e28e28-5652-497d-aea8-3e611e9a077b').
narrative_ontology:cs_reading_relation('c5e28e28-5652-497d-aea8-3e611e9a077b', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5e28e28-5652-497d-aea8-3e611e9a077b', ietf_openness_commitment__legitimacy_erosion_reading, influences).
narrative_ontology:cs_axiom('c5e28e28-5652-497d-aea8-3e611e9a077b', foundational, participation_weight_tracks_resources).
narrative_ontology:cs_axiom_status(participation_weight_tracks_resources, holdable).
narrative_ontology:cs_axiom_grounding('c5e28e28-5652-497d-aea8-3e611e9a077b', participation_weight_tracks_resources, empirically_contingent).
narrative_ontology:cs_axiom('c5e28e28-5652-497d-aea8-3e611e9a077b', secondary, openness_label_laundering).
narrative_ontology:cs_axiom_status(openness_label_laundering, holdable).
narrative_ontology:cs_axiom_grounding('c5e28e28-5652-497d-aea8-3e611e9a077b', openness_label_laundering, empirically_contingent).
narrative_ontology:cs_reference_frame('c5e28e28-5652-497d-aea8-3e611e9a077b', resource_blind_open_participation).
narrative_ontology:cs_drift_state('c5e28e28-5652-497d-aea8-3e611e9a077b', post_platform_consolidation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c5e28e28-5652-497d-aea8-3e611e9a077b', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, hyperscale_platform_operators).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, essential_patent_holders).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, small_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, independent_software_vendors).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, end_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, end_users).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__capture_substrate_reading, rough_consensus_running_code_doctrine).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__capture_substrate_reading, vendor_neutral_proceduralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the deployments that give their engineering choices de facto force. Send large standing delegations to working groups, author a plurality of drafts, hold chair and area-director positions, and run internal alignment before meetings so their positions arrive consolidated. Specifications that encode their architectures and complexity that raises rivals' compliance costs both flow to their advantage. They can shift standardization venues or standardize by deployment alone, so no single process binds them.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, hyperscale_platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__capture_substrate_reading, hyperscale_platform_operators, beneficiary).

% Contribute patented techniques into candidate specifications under voluntary licensing declarations, then negotiate terms bilaterally with each implementer from a position of essentiality. Collect per-unit revenue on compliant products; disclosure timing and claim drafting shape what becomes essential. Can enforce through litigation against implementers who dispute terms, and can assert the same portfolio in any standards venue.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, essential_patent_holders, beneficiary,
    powerful, biographical, mobile, global).

% Build products that must interoperate with the installed base, so they implement the specification as written — including an extension surface sized to large operators' needs. Track extensive normative text with engineering teams far smaller than the contributors'; cannot sustain continuous presence in working groups and typically learn of decisions after they harden. Nonconformance means losing interoperability with their customers, so leaving the specification means leaving their market.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, small_implementers, payer,
    moderate, immediate, constrained, regional).

% Ship commercial software implementing the standardized protocols and pay licensing rates set in confidential bilateral negotiation with patent holders holding essentiality. Certification and compliance testing add fixed costs that weigh heaviest on thin-margin vendors. The standard defines the market they sell into, so exiting the standard is exiting the market.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, independent_software_vendors, payer,
    moderate, biographical, constrained, global).

% Receive working interoperability between devices and services — the arrangement's headline deliverable — while absorbing lock-in: proprietary extensions fragment their experience, switching costs compound with each adopted ecosystem, and the field of implementers narrows as compliance burdens consolidate it. Hold no seat in the process; their influence arrives only as aggregated market behavior, and coordinated action among them is latent rather than organized.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, end_users, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__capture_substrate_reading, end_users, beneficiary).

% Approve specifications for publication, appoint working-group chairs, and administer the published process rules. Their authority is formal, but the content they ratify arrives pre-shaped by the participants with the deepest benches. Their professional standing is constituted by stewardship of an open process; characterizing the process as captured would dissolve the identity that makes the role worth holding, so the possibility is structurally difficult for them to entertain.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, ietf_leadership_iesg, agenda_setter,
    institutional, generational, identity_locked, global).

% Civil-society technologists, security researchers outside corporate laboratories, and accessibility specialists who would press privacy, safety, and accessibility requirements into specifications. In-person meeting economics and multi-year working-group commitments price out sustained participation, and no alternative venue holds comparable agenda power over these specifications — so their exclusion is maintained by the same participation structure the process runs on.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, public_interest_advocates, excluded,
    powerless, generational, trapped, global).

% Investigate whether licensing conduct around standard-essential patents and dominance of process agendas amount to competitive harm. Take evidence from implementers and patent holders, commission economic analysis, and can impose remedies on licensing terms — though their instruments reach conduct downstream of the process rather than the process's internal structure.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, competition_authorities, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__capture_substrate_reading, hyperscale_platform_operators).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__capture_substrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: getting competing vendors to agree on wire protocols, message formats, and state machines so heterogeneous systems interoperate without pairwise bilateral negotiation, and maintaining a neutral venue where such agreement can be recorded and revised.
% TRANSFER_FUNCTION: Moves agenda control and specification content toward participants with the resources to sustain presence; moves licensing revenue from implementers to patent holders; moves attention, procedural office, and reputational standing within the process toward those who can fund continuous engagement.
% ABSENT_VOICES: Public-interest advocates, unfunded engineers from small firms, and independent security researchers would object to the specification priorities and licensing exposure the process produces; they are absent because meeting economics and multi-year engagement demands exclude them, and no rival venue offers comparable agenda power over the same specifications.
% DISAPPEARANCE_RATIONALE: If the process and its enforcement vanished overnight, standardization would not stop — it would reorganize around de facto deployment standards controlled by whichever operators ship at scale, with licensing asserted ad hoc and no recorded neutral venue for revision. Small implementers would lose the one forum where their objections can at least be registered; the interoperability layer would persist but its governance would privatize.
% FOUNDING_PROBLEM: A fragmented protocol landscape: vendor-proprietary network protocols, incompatible networks that could not exchange traffic, and no neutral venue where competing organizations could agree on shared specifications.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the documented pre-standardization fragmentation record (vendor-proprietary networking of the 1970s–80s, chronicled in academic histories of internetworking), by early participants now unaffiliated with current beneficiaries, and by competition-authority economic analyses treating baseline interoperability at the transport layer as a solved problem. The benefiting parties attest continued liveness for new application domains; no source outside the benefiting parties attests that the original founding problem still governs the process's current form.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__capture_substrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__capture_substrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__capture_substrate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.58 (moderate-to-substantial): the process delivers real coordination, but the distribution of specification content, extension rights, and licensing exposure is systematically steered toward the resource-rich. Suppression is authored at 0.46 as a raw structural property — it is NOT scaled by power or scope in the engine's computation; only extractiveness is scaled. The suppressive force here is structural rather than coercive: meeting economics, procedural mastery requirements, and licensing exposure raise the cost of effective participation for smaller actors without prohibiting anyone from attending. Theater ratio 0.34: consensus calls, disclosure rituals, and openness affirmations are partly functional and increasingly performative relative to outcomes. Accessibility collapse 0.40: alternatives persist (other standards bodies, de facto deployment standardization, open-source forks), so the constraint does not foreclose escape — the signature of a hybrid rather than a pure extraction structure. Resistance 0.52: sustained pushback from litigation, fork movements, reform proposals, and rival consortia. The claimed type (tangled_rope) is stated from structure — genuine coordination function plus asymmetric extraction plus active enforcement — independently of the metric values; the engine computes per-seat classifications from the structural data, and any divergence between claim and computed type is the measurement the corpus exists to take. All three temporal series run on one shared grid (t=0,8,16,24,32,39) so every metric is authored at every examined time point; the rising suppression_requirement series traces the hardening of the process's gating machinery (formalized procedures, disclosure regimes, attendance economics) over the interval, which is why it is authored despite a stable-enforcement baseline being available. Receipt surface: the arrangement's gains demonstrably accrue to the hyperscaler seat — agenda control, architectural entrenchment, deployment-scale advantage — with patent holders receiving a measurable but subordinate royalty stream, so gain_flow names hyperscale_platform_operators rather than diffuse. Fixing is prohibitive: participation-economics reform must be enacted through bodies whose agendas the current dominant participants already shape.
 *
 * PERSPECTIVAL GAP:
 *   From the hyperscaler seat the process is a fair exchange: these actors fund it, staff it, and receive specifications their operations require — coordination they experience as earned contribution. From the small-implementer seat the same machinery is a gate paid to pass: rules written at a cadence and scale they cannot match, licensing terms negotiated from weakness, decisions learned after they harden. The formal leadership seat experiences the process as neutral administration; admitting the capture characterization would contradict the professional identity that makes the seat worth holding, which is why that seat is authored identity_locked. Same-nominal-power divergence: small_implementers and independent_software_vendors both sit at moderate power with constrained exit, but differ in horizon and scope — the former fight specification complexity quarter to quarter in regional markets, the latter amortize royalty stacks across global product lines — so the engine should compute different effective extraction for nominally equal seats. Coalition potential: end_users are individually powerless but their aggregated market behavior is the one lever that has historically moved specification politics; the analysis treats user-class coordination as latent rather than absent.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for hyperscale_platform_operators (their arbitrage-grade exit — multi-venue and deployment-first strategies — pushes them toward the full-beneficiary end) and for essential_patent_holders (mobile exit: portfolios can be asserted in any venue). Victim declarations drive high directionality for small_implementers and independent_software_vendors, amplified by constrained exit: conformance to the specification as written is the price of remaining in the market the specification defines. end_users derive high directionality from victim status, but their dual beneficiary position — interoperability is the arrangement's headline deliverable to them — places them nearer symmetric; the engine resolves this from the declared dual role. public_interest_advocates sit outside the beneficiary/victim derivation entirely: their exclusion is the enforcement object itself, not a directional position. Global spatial scope amplifies effective extraction modestly for the payer seats, since verification of specification intent and licensing fairness is harder at planetary scale.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fragmented, vendor-proprietary network protocols with no neutral agreement venue — was substantially solved at the transport and network layers, yet the process persists with an expanded mandate into application and web domains where the problem's liveness is disputed. The classification prevents mislabeling in both directions: a pure extraction reading fails because the coordination product is real and would be reproduced worse (privately, by deployment fiat) without the process; a pure coordination reading fails because the output distribution is steered and actively defended. Mandatrophy resolves as contested rather than dead: the original problem is gone at the layers that motivated the founding, but successor problems keep parts of the mandate load-bearing. The mismatch consumer reads founding_problem_status=contested together with disappearance_verdict=world_rearranges: arrangements demonstrably depend on the process, so no zombie flag fires — the persistence is explained by dependence, not by inertia alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the ietf_openness_commitment kernel. Is the standards process''s structure accurately read as a capture substrate (this reading), as commons stewardship, or as a legitimacy-erosion problem — and which structural elements would each sibling reading change?',
    'Comparative classification across the three reading-stories sharing the kernel: if the commons_stewardship_reading''s epsilon sits near the coordination-cost floor with no concentrated beneficiary, this reading overstates extraction; if the legitimacy_erosion_reading locates the defect in procedural safeguards rather than resource asymmetry, the intervention locus shifts from participation economics to safeguard repair.',
    'Resolved toward commons stewardship, epsilon falls toward the global_infrastructure floor and the type trends rope; resolved toward legitimacy erosion, the victim set persists but blame migrates from resource-rich participants to the agenda_setter seat; unresolved, per-seat classifications diverge permanently across the family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the openness commitment the process structure instantiates.').

omega_variable(
    expertise_or_resource_causality,
    'Is the encoded gatekeeping caused by resource advantage converting to agenda control, or by genuine protocol expertise concentrating in exactly the firms with the resources to participate?',
    'Within-firm comparisons of specification influence between equally-resourced teams with different protocol depth; cross-domain comparison where expertise is dispersed but participation resources are not.',
    'If expertise-causal, a substantial share of measured extraction is the price of specification quality and the constraint sits closer to rope; if resource-causal, funding-neutral participation mechanisms would reduce epsilon without quality loss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expertise_or_resource_causality, empirical, 'Whether gatekeeping reflects resources or the expertise correlated with them.').

omega_variable(
    frand_terms_opaqueness,
    'What do implementers actually pay under FRAND commitments, given that license terms are set bilaterally under confidentiality?',
    'Adjudicated rate-setting cases, disclosed portfolio benchmarks, and aggregate royalty-stack accounting across a representative product category.',
    'A royalty stack materially above disclosed RAND benchmarks raises epsilon and strengthens the payer-seat reading; rates near benchmark support the coordination-cost framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frand_terms_opaqueness, empirical, 'Actual versus committed licensing burden on implementers.').

omega_variable(
    counterfactual_venue_quality,
    'Would small implementers and users be better served by the counterfactual in which no open process exists and standards emerge from deployment alone?',
    'Historical natural experiments comparing protocol domains standardized by open process against adjacent domains standardized by deployment; compare entry rates, royalty incidence, and fragmentation costs.',
    'If the counterfactual is worse for small players, part of the measured extraction is the least-bad-equilibrium price of having a process at all — supporting tangled_rope over snare; if comparable or better, the process adds gatekeeping without protective value.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_venue_quality, conceptual, 'Quality of the no-process counterfactual for the constraint''s targets.').

omega_variable(
    openness_identity_internalization,
    'Is the suppression that keeps targets quiescent purely structural (meeting economics, procedural mastery, licensing exposure), or partly internalized — participants and observers accepting the openness self-image and therefore not perceiving the gate?',
    'Post-exit assessment studies: whether implementers who leave the process revise their evaluation of its openness; systematic comparison of participant beliefs against measured agenda-control distributions.',
    'If internalized, effective suppression exceeds the structural measure — targets defend the arrangement that gates them and reform demand stays weak as extraction rises; if purely structural, removing participation-economics barriers should close the gap quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(openness_identity_internalization, conceptual, 'Structural versus internalized component of the process''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__capture_substrate_reading, 0, 39).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_capture_substrate_tr_t0, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(ietf_capture_substrate_tr_t0, observed).
narrative_ontology:measurement(ietf_capture_substrate_tr_t8, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement_basis(ietf_capture_substrate_tr_t8, observed).
narrative_ontology:measurement(ietf_capture_substrate_tr_t16, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement_basis(ietf_capture_substrate_tr_t16, observed).
narrative_ontology:measurement(ietf_capture_substrate_tr_t24, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement_basis(ietf_capture_substrate_tr_t24, observed).
narrative_ontology:measurement(ietf_capture_substrate_tr_t32, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 32, 0.31).
narrative_ontology:measurement_basis(ietf_capture_substrate_tr_t32, observed).
narrative_ontology:measurement(ietf_capture_substrate_tr_t39, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 39, 0.34).
narrative_ontology:measurement_basis(ietf_capture_substrate_tr_t39, observed).

% Extraction over time
narrative_ontology:measurement(ietf_capture_substrate_be_t0, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(ietf_capture_substrate_be_t0, observed).
narrative_ontology:measurement(ietf_capture_substrate_be_t8, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement_basis(ietf_capture_substrate_be_t8, observed).
narrative_ontology:measurement(ietf_capture_substrate_be_t16, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement_basis(ietf_capture_substrate_be_t16, observed).
narrative_ontology:measurement(ietf_capture_substrate_be_t24, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement_basis(ietf_capture_substrate_be_t24, observed).
narrative_ontology:measurement(ietf_capture_substrate_be_t32, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement_basis(ietf_capture_substrate_be_t32, observed).
narrative_ontology:measurement(ietf_capture_substrate_be_t39, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 39, 0.58).
narrative_ontology:measurement_basis(ietf_capture_substrate_be_t39, observed).

% Suppression requirement over time
narrative_ontology:measurement(ietf_capture_substrate_su_t0, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(ietf_capture_substrate_su_t0, observed).
narrative_ontology:measurement(ietf_capture_substrate_su_t8, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 8, 0.26).
narrative_ontology:measurement_basis(ietf_capture_substrate_su_t8, observed).
narrative_ontology:measurement(ietf_capture_substrate_su_t16, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 16, 0.33).
narrative_ontology:measurement_basis(ietf_capture_substrate_su_t16, observed).
narrative_ontology:measurement(ietf_capture_substrate_su_t24, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 24, 0.39).
narrative_ontology:measurement_basis(ietf_capture_substrate_su_t24, observed).
narrative_ontology:measurement(ietf_capture_substrate_su_t32, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 32, 0.43).
narrative_ontology:measurement_basis(ietf_capture_substrate_su_t32, observed).
narrative_ontology:measurement(ietf_capture_substrate_su_t39, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 39, 0.46).
narrative_ontology:measurement_basis(ietf_capture_substrate_su_t39, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__capture_substrate_reading, global_infrastructure).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__legitimacy_erosion_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the IETF's openness commitment' decomposes into three structurally distinct readings of one kernel. This story authors the capture_substrate_reading (moderate epsilon, concentrated beneficiaries, encoded gatekeeping); commons_stewardship_reading authors the same process as low-epsilon public infrastructure; legitimacy_erosion_reading authors the rough-consensus mechanism itself as the contested element. Each member carries its own epsilon, beneficiary set, and victims per the epsilon-invariance principle; the edges declared here are family links establishing kinship, not causal claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
