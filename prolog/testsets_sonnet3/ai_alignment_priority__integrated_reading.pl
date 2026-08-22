% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__integrated_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ai_alignment_priority__integrated_reading
 *   human_readable: Integrated Alignment Priority: Catastrophic and Present Harms as Complementary
 *   domain: AI governance/technology ethics/risk assessment
 *
 * SUMMARY:
 *   This story instantiates the 'integrated reading' of the AI alignment
 *   priority kernel: the claim that catastrophic capability risk and present
 *   deployment harm are complementary rather than competing priorities, and
 *   that governance institutions should fund and evaluate both under one
 *   framework using dual methodology (red-teaming for capability risk, audits
 *   for deployment harm). This is a distinct constraint from the
 *   existential_risk_reading (priority: preventing catastrophic loss of
 *   control) and the nearterm_harms_reading (priority: justice for
 *   marginalized populations against present algorithmic harm) — those are
 *   separate stories with their own epsilon and their own victim sets, linked
 *   here via network.affects_constraints. The integrated reading's own
 *   metrics describe moderate extraction on both fronts: large frontier labs
 *   and the professional class that administers the dual framework capture
 *   disproportionate benefit (institutional legitimacy, funding
 *   diversification, reduced accountability to either advocacy camp) while
 *   both present-day marginalized communities and future populations bear
 *   real, if moderate, costs from resource dilution relative to what a
 *   single-track priority would have delivered to each.
 *
 * KEY AGENTS:
 *   - large_frontier_labs: agenda-setting beneficiary that administers the dual-track allocation and gains legitimacy from claiming to serve both priorities
 *   - ai_governance_professional_class: beneficiary whose career structure depends on the integrated paradigm persisting
 *   - marginalized_deployment_affected_communities: present-day victim bearing diluted audit depth
 *   - future_populations_under_capability_risk: non-agent victim proxied by researchers, bearing diluted capability-risk research depth
 *   - existential_risk_advocates and nearterm_justice_advocates: excluded single-track camps whose sharper priority framings are structurally outvoted
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, 0.46).
domain_priors:suppression_score(ai_alignment_priority__integrated_reading, 0.42).
domain_priors:theater_ratio(ai_alignment_priority__integrated_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__integrated_reading, "Integrated Alignment Priority: Catastrophic and Present Harms as Complementary").
narrative_ontology:topic_domain(ai_alignment_priority__integrated_reading, "AI governance/technology ethics/risk assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__integrated_reading, '448ef9ea-f142-4424-b9e8-c8add211c717').
narrative_ontology:cs_kernel_codification('448ef9ea-f142-4424-b9e8-c8add211c717', distributed).
narrative_ontology:cs_authority_grounding('448ef9ea-f142-4424-b9e8-c8add211c717', distributed).
narrative_ontology:cs_reading_relation('448ef9ea-f142-4424-b9e8-c8add211c717', ai_alignment_priority__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('448ef9ea-f142-4424-b9e8-c8add211c717', ai_alignment_priority__nearterm_harms_reading, influences).
narrative_ontology:cs_axiom('448ef9ea-f142-4424-b9e8-c8add211c717', foundational, harm_priorities_are_jointly_addressable_without_tradeoff).
narrative_ontology:cs_axiom_status(harm_priorities_are_jointly_addressable_without_tradeoff, holdable).
narrative_ontology:cs_axiom_grounding('448ef9ea-f142-4424-b9e8-c8add211c717', harm_priorities_are_jointly_addressable_without_tradeoff, instrumental).
narrative_ontology:cs_axiom('448ef9ea-f142-4424-b9e8-c8add211c717', secondary, resource_parity_between_harm_categories_is_the_correct_default).
narrative_ontology:cs_axiom_status(resource_parity_between_harm_categories_is_the_correct_default, holdable).
narrative_ontology:cs_axiom_grounding('448ef9ea-f142-4424-b9e8-c8add211c717', resource_parity_between_harm_categories_is_the_correct_default, conventional).
narrative_ontology:cs_reference_frame('448ef9ea-f142-4424-b9e8-c8add211c717', dual_track_governance_consensus).
narrative_ontology:cs_drift_state('448ef9ea-f142-4424-b9e8-c8add211c717', post_2023_ai_governance_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('448ef9ea-f142-4424-b9e8-c8add211c717', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__integrated_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, ai_governance_professional_class).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, large_frontier_labs).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, policy_intermediary_institutions).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, marginalized_deployment_affected_communities).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, future_populations_under_capability_risk).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, under_resourced_safety_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set internal alignment research agendas and public governance commitments, allocating budget across red-teaming for catastrophic capability risks and audits for deployment harms. Their public commitment to 'both matter' lets them claim credit on both fronts while retaining discretion over the actual split, which in practice skews toward capability work that also advances competitive model performance.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, large_frontier_labs, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__integrated_reading, large_frontier_labs, beneficiary).

% Career researchers, policy staff, and conference/institute personnel whose professional standing depends on the integrated frame remaining the dominant paradigm — it justifies dual funding streams, cross-disciplinary hiring, and convening power. Mobile between labs, academia, and policy shops; their expertise is portable regardless of which harm the money ultimately funds.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, ai_governance_professional_class, beneficiary,
    organized, biographical, mobile, global).

% Standards bodies, think tanks, and multilateral working groups that produce the audit-plus-red-team methodology this reading requires. They benefit from being the indispensable synthesizers of a two-track agenda, but are constrained by donor and member-state priorities that frequently pull toward one track over the other.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, policy_intermediary_institutions, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__integrated_reading, policy_intermediary_institutions, agenda_setter).

% Bear discriminatory hiring, lending, policing, and content-moderation harms from deployed systems today. Under the integrated frame their harms compete for a share of finite audit resources against speculative future-catastrophe research; they cannot opt out of algorithmic systems already governing housing, credit, or benefits decisions, and have no seat in prioritization decisions made at the lab or standards-body level.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, marginalized_deployment_affected_communities, payer,
    powerless, immediate, trapped, national).

% Cannot be represented directly; their stake is proxied by present-day existential-risk researchers. Under the integrated frame their interests are asserted to be served by a share of resources, but the share is set by institutions with no accountability mechanism running from these populations back to the allocators.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, future_populations_under_capability_risk, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(ai_alignment_priority__integrated_reading, future_populations_under_capability_risk).

% Independent and academic researchers working on either track who must frame proposals as serving 'both' priorities to remain fundable within the integrated paradigm, even when their actual expertise and evidence base is narrowly present-harms or narrowly capability-risk focused. The requirement to perform integration diverts effort from depth.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, under_resourced_safety_researchers, payer,
    moderate, biographical, constrained, national).

% Hold that the integrated frame dilutes urgent capability-risk work by mandating parity with present-harms funding streams; they are present in the discourse but structurally outvoted whenever governance bodies adopt balanced-allocation defaults.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, existential_risk_advocates, excluded,
    organized, civilizational, constrained, global).

% Hold that the integrated frame launders continued deployment of harmful systems by giving equal rhetorical weight to speculative future catastrophe alongside documented present injury; they participate in the same forums but their priority framing is treated as one input among several rather than as the governing one.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, nearterm_justice_advocates, excluded,
    organized, immediate, constrained, global).

% Conduct third-party audits and academic analysis of whether integrated-frame institutions actually balance resources as claimed, or whether the balance is asserted rhetorically while allocation skews toward whichever track advances institutional or commercial interests.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, independent_ai_ethics_auditors, observer,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__integrated_reading, large_frontier_labs).
narrative_ontology:fixing_cost_class(ai_alignment_priority__integrated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single governance vocabulary and resource-allocation framework under which capability-risk researchers and present-harms researchers can be funded from the same institutions, cited in the same policy documents, and evaluated by the same standards bodies, instead of running two disconnected and mutually hostile advocacy movements.
% TRANSFER_FUNCTION: Moves funding, institutional attention, and regulatory bandwidth between two harm categories according to allocation decisions made by labs and standards bodies; in practice tends to move a disproportionate share toward capability-risk work (which overlaps with commercially valuable capability research) while claiming parity, at the expense of present-harms audit depth and unrepresented future-population interests.
% ABSENT_VOICES: Communities currently harmed by deployed systems (housing, hiring, policing algorithms) rarely sit on the technical committees that set the actual dollar split; future populations have no representative mechanism at all beyond researcher proxy claims. Both existential-risk-only and nearterm-harms-only advocates are present in the discourse but treated as partisans of a narrower frame rather than as the deciding voice.
% DISAPPEARANCE_RATIONALE: If the integrated framing vanished, some institutions would default to whichever single-track priority their existing funders favor (likely capability risk for frontier labs, present harms for civil-society-funded bodies), while others argue the underlying dual-track funding and dual-methodology infrastructure (red-teaming plus audits) would persist regardless of the rhetorical frame, since both methods are independently useful. The parties dispute whether the frame is load-bearing or merely descriptive of an allocation that would happen anyway.
% FOUNDING_PROBLEM: Two rival AI-safety advocacy communities were competing for the same scarce policy attention and philanthropic funding, each arguing the other's priority was a distraction; the integrated frame was built to stop that competition from fragmenting the field and to let institutions fund both without choosing.
% FOUNDING_PROBLEM_CORROBORATION: Governance-professional and policy-intermediary seats attest the tension is durably real and the integrated frame remains necessary; independent auditors and both excluded advocacy camps attest from outside the beneficiary set that the 'complementary, not competing' framing functions in practice as a resource-allocation compromise that under-serves both — present-harms audits are shallower than dedicated nearterm advocacy would produce, and capability-risk work is more diffuse than dedicated existential-risk advocacy would produce.
narrative_ontology:disappearance_verdict(ai_alignment_priority__integrated_reading, contested).
narrative_ontology:founding_problem_status(ai_alignment_priority__integrated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__integrated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_priority__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__integrated_reading, 0.46, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__integrated_reading_tests).
:- end_tests(ai_alignment_priority__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.46 (rising from 0.30) reflecting the story's own view that the integrated frame's institutional overhead grows over time as more allocators adopt it and as its administrative apparatus (joint standards, cross-track reporting) matures — genuinely serving both tracks less well than either single-track advocacy would serve its own. Suppression (0.42) is moderate: the integrated frame does not coercively silence either advocacy camp, but it does structurally discount sharper single-priority arguments in convening and funding decisions, which functions as soft suppression of the excluded voices. Theater ratio rises to 0.38, reflecting the story's claim that 'balanced allocation' is increasingly asserted rhetorically by institutions whose actual dollar splits skew toward capability work that also advances commercial capability development — a genuine but partial theater component, not a certification of pure performance.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting lab's seat, the integrated frame looks like responsible, comprehensive governance. From either excluded advocacy camp's seat, or from the seat of a present-day harmed community or a proxied future population, the same structure looks like a mechanism for diluting accountability on both fronts simultaneously while institutions collect the reputational benefit of appearing to take both seriously. The engine computes these as different per-seat classifications from the same structural data; this story does not adjudicate which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   Large frontier labs and the professional/intermediary class sit near the beneficiary end: they administer the allocation, gain legitimacy and career/institutional stability from the framework's persistence, and face no binding accountability if the claimed balance is not delivered. Present-day marginalized communities and future populations sit near the target end: both bear diluted resourcing relative to a single-track counterfactual, and both are structurally distant from the allocation decision — one trapped in immediate exposure to deployed systems, the other unable to participate at all and proxied only by researcher advocacy. Under-resourced researchers sit in between: moderate power, constrained exit, forced to perform integration to remain fundable.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than pure snare or pure rope matters here: the integrated frame does solve a genuine coordination problem (stopping two advocacy movements from cannibalizing each other's funding and policy attention), which is why it is not a pure extraction story. But the same coordination structure lets the administering institutions extract legitimacy and resource discretion without being held to either track's own standard of adequacy — that is the asymmetric extraction that makes it tangled rather than a clean rope. Treating this as pure coordination (rope) would erase the real cost to both excluded camps; treating it as pure extraction (snare) would erase the genuine value of not having AI safety funding capsize into zero-sum advocacy war.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_versus_dilution,
    'Does joint resourcing of catastrophic and present-harm alignment work genuinely serve both better than they would be served by dedicated single-track institutions, or does it structurally dilute both in service of institutional legitimacy and reduced accountability?',
    'Comparative outcome tracking: measure audit depth and remediation rates for present-harms work, and capability-risk research output and red-team coverage, in integrated-frame institutions versus matched single-track institutions over a multi-year window.',
    'If integration measurably improves outcomes on both tracks relative to single-track counterfactuals, the tangled_rope classification should weight toward the rope side (genuine coordination dominates). If it measurably dilutes both, the classification should weight toward snare (the coordination story is functioning primarily as cover for reduced accountability).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(integration_versus_dilution, empirical, 'Whether integration is complementary in practice or a resource-dilution mechanism dressed as complementarity.').

omega_variable(
    future_population_proxy_legitimacy,
    'Can any present-day institutional arrangement legitimately claim to represent the interests of future populations under capability risk, given the total absence of a feedback or accountability channel running from those populations to current allocators?',
    'Compare against other domains with analogous representation problems (long-term environmental policy, intergenerational fiscal commitments) to see whether any institutional design has produced verifiable proxy accountability, versus remaining permanently unverifiable in principle.',
    'If no legitimate proxy is possible even in principle, the future_populations_under_capability_risk victim designation is more accurately described as a rhetorical placeholder used to justify capability-risk allocation than as a genuine represented interest — this would shift weight toward the nearterm_harms_reading''s critique that the integrated frame''s future-harm claims are unfalsifiable and thus function as a blank check.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_population_proxy_legitimacy, conceptual, 'Whether future-population interests can be genuinely represented in present resource-allocation decisions.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the ''integrated reading'' itself a distinct structural position, or is it better understood as the institutional compromise that emerges whenever neither single-track advocacy camp can capture full control of a governance body — i.e., an equilibrium outcome rather than a first-order normative claim about what alignment ''requires''?',
    'Trace the genealogy of specific integrated-frame institutions: were they founded on an explicit philosophical argument for complementarity, or did they emerge from negotiated funding settlements between competing advocacy coalitions? Document founding charters and early funding negotiations.',
    'If integration is primarily an equilibrium outcome of coalition bargaining rather than a philosophically grounded position, this story''s claimed_type and epsilon should be read as describing a governance compromise''s structural properties, not as adjudicating a genuine philosophical claim about what alignment requires — this would not change the authored metrics but would reframe how the reading is cited relative to its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the integrated reading is a first-order normative position or a negotiated institutional equilibrium between the two single-track readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__integrated_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__integrated_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_priority__integrated_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_priority__integrated_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_priority__integrated_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_priority__integrated_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__integrated_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(ai_a_tr_t24, ai_alignment_priority__integrated_reading, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__integrated_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_priority__integrated_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_priority__integrated_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_priority__integrated_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_priority__integrated_reading, base_extractiveness, 16, 0.43).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__integrated_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(ai_a_be_t24, ai_alignment_priority__integrated_reading, base_extractiveness, 24, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__integrated_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_priority__integrated_reading, suppression_requirement, 4, 0.29).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_priority__integrated_reading, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_priority__integrated_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_priority__integrated_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__integrated_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(ai_a_su_t24, ai_alignment_priority__integrated_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__integrated_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__integrated_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__nearterm_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposed from the natural-language label 'AI alignment priority' per the ε-invariance principle. Each reading names a different victim set and a different resource-allocation logic and therefore carries its own ε: existential_risk_reading (higher ε on capability-risk-specific extraction, victim set is future populations only), integrated_reading (this story; moderate ε on both fronts, victim set spans both present marginalized communities and future populations), nearterm_harms_reading (higher ε on present deployment-harm-specific extraction, victim set is present marginalized populations only). All three share the same kernel (ai_alignment_priority) but are not measurement-parameter variants of one constraint — they are structurally distinct constraints with different beneficiary/victim structures, linked here for contamination-propagation analysis: institutional adoption or discrediting of the integrated frame directly affects funding availability and legitimacy conditions for both single-track siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
