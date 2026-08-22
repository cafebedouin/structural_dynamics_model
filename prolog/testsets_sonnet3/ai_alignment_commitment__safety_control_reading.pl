% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__safety_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__safety_control_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: ai_alignment_commitment__safety_control_reading
 *   human_readable: AI Alignment as Catastrophic Loss-of-Control Prevention
 *   domain: AI governance / existential risk / technology ethics
 *
 * SUMMARY:
 *   Major AI labs, existential-risk research institutes, and a cluster of
 *   philanthropic funders have converged on defining 'AI alignment' primarily
 *   as the problem of preventing advanced systems from acting in ways that
 *   permanently escape meaningful human correction or oversight — the
 *   loss-of-control catastrophe. This framing genuinely names a real
 *   coordination problem (sufficiently capable autonomous systems could in
 *   principle resist correction) but its dominance in funding, policy
 *   language, and research agenda-setting also diverts resources and
 *   legitimacy away from auditing and remediating present-day algorithmic
 *   harms borne by relatively powerless populations. The same institutional
 *   structure that funds genuine technical safety work also uses the
 *   catastrophic framing to justify self-regulation, scale, and reduced
 *   third-party scrutiny of currently deployed systems.
 *
 * KEY AGENTS:
 *   - frontier_ai_labs: primary agenda-setter and beneficiary (institutional/arbitrage) — sets what counts as alignment work, benefits from self-regulation legitimacy
 *   - existential_risk_research_institutes: beneficiary (organized/arbitrage) — funding and legitimacy tied to catastrophic framing dominance
 *   - present_day_algorithmic_harm_communities: primary target (powerless/trapped) — bear extraction of attention and resources away from present remediation
 *   - global_south_ai_deployment_populations: secondary target (powerless/constrained) — under-resourced context-specific harm mitigation
 *   - ai_safety_research_funding_competitors: secondary target (moderate/constrained) — compete for finite attention against catastrophic-framing dominance
 *   - ai_policy_regulators: analytical observer (institutional/analytical) — adjudicates which framing anchors statutory language
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__safety_control_reading, 0.62).
domain_priors:suppression_score(ai_alignment_commitment__safety_control_reading, 0.48).
domain_priors:theater_ratio(ai_alignment_commitment__safety_control_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__safety_control_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__safety_control_reading, "AI Alignment as Catastrophic Loss-of-Control Prevention").
narrative_ontology:topic_domain(ai_alignment_commitment__safety_control_reading, "AI governance / existential risk / technology ethics").

domain_priors:requires_active_enforcement(ai_alignment_commitment__safety_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__safety_control_reading, 'd2cd0d11-ab3e-4643-a1e9-5da06320f356').
narrative_ontology:cs_kernel_codification('d2cd0d11-ab3e-4643-a1e9-5da06320f356', distributed).
narrative_ontology:cs_authority_grounding('d2cd0d11-ab3e-4643-a1e9-5da06320f356', distributed).
narrative_ontology:cs_reading_relation('d2cd0d11-ab3e-4643-a1e9-5da06320f356', ai_alignment_commitment__ethics_justice_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2cd0d11-ab3e-4643-a1e9-5da06320f356', ai_alignment_commitment__integrated_reading, influences).
narrative_ontology:cs_axiom('d2cd0d11-ab3e-4643-a1e9-5da06320f356', foundational, loss_of_control_is_the_dominant_catastrophic_risk).
narrative_ontology:cs_axiom_status(loss_of_control_is_the_dominant_catastrophic_risk, holdable).
narrative_ontology:cs_axiom_grounding('d2cd0d11-ab3e-4643-a1e9-5da06320f356', loss_of_control_is_the_dominant_catastrophic_risk, empirically_contingent).
narrative_ontology:cs_axiom('d2cd0d11-ab3e-4643-a1e9-5da06320f356', foundational, speculative_future_harm_warrants_present_resource_priority).
narrative_ontology:cs_axiom_status(speculative_future_harm_warrants_present_resource_priority, holdable).
narrative_ontology:cs_axiom_grounding('d2cd0d11-ab3e-4643-a1e9-5da06320f356', speculative_future_harm_warrants_present_resource_priority, instrumental).
narrative_ontology:cs_reference_frame('d2cd0d11-ab3e-4643-a1e9-5da06320f356', existential_risk_primacy_framework).
narrative_ontology:cs_drift_state('d2cd0d11-ab3e-4643-a1e9-5da06320f356', post_frontier_scaling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d2cd0d11-ab3e-4643-a1e9-5da06320f356', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, existential_risk_research_institutes).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, future_generations).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, present_day_algorithmic_harm_communities).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, global_south_ai_deployment_populations).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, ai_safety_research_funding_competitors).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__safety_control_reading, instrumental_convergence_thesis).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__safety_control_reading, orthogonality_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the research agenda for what counts as 'alignment work,' fund internal safety teams framed around catastrophic misalignment, and use the control-loss framing to justify scale, secrecy, and self-regulation (we are the only ones careful enough to build this safely). Their competitive position is enhanced by a framing that makes deployment pauses and third-party audits look reckless relative to internal caution.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, frontier_ai_labs, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__safety_control_reading, frontier_ai_labs, beneficiary).

% Receive philanthropic and lab funding, academic legitimacy, and policy access premised on the catastrophic-control framing being the central alignment problem. Their institutional survival and career pipelines are structurally tied to future, low-probability, high-severity scenarios remaining the field's defining concern.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, existential_risk_research_institutes, beneficiary,
    organized, civilizational, arbitrage, global).

% Cannot advocate for themselves; are named as the ultimate beneficiaries of control-preserving measures taken today. Whether they actually benefit depends on whether the catastrophic scenario the framing prioritizes is the one that would otherwise materialize.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, future_generations, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ai_alignment_commitment__safety_control_reading, future_generations).

% Experience discriminatory lending, biased hiring screens, wrongful facial-recognition arrests, and content-moderation harms from deployed systems today. Research funding, regulatory attention, and lab safety-team headcount directed toward speculative catastrophic scenarios is funding and attention not directed toward auditing and remediating the harms they currently absorb. They have no exit from systems already deployed against them.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, present_day_algorithmic_harm_communities, payer,
    powerless, immediate, trapped, global).

% Are subject to AI systems built and safety-tested primarily against Western catastrophic-risk benchmarks, with far less investment in context-specific present-harm evaluation (language coverage, labor displacement, surveillance misuse). The safety-control framing's dominant funding claim leaves fewer resources for locally relevant harm mitigation.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, global_south_ai_deployment_populations, payer,
    powerless, generational, constrained, global).

% Researchers working on bias auditing, labor impact, or present-day fairness compete for the same finite grant pools and policy attention as catastrophic-risk researchers. When the safety-control reading dominates institutional agenda-setting, their proposals are read as lower priority or as a distraction from the 'real' problem.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, ai_safety_research_funding_competitors, payer,
    moderate, biographical, constrained, national).

% Draft legislation and standards under lobbying pressure from labs who frame catastrophic-control risk as the paramount concern justifying self-regulation, while civil-society groups push present-harm evidence. Regulators must choose which framing anchors statutory language and enforcement priorities.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, ai_policy_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__safety_control_reading, frontier_ai_labs).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__safety_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a genuine collective-action problem: if frontier AI systems could act with sufficient autonomy and capability to resist correction, no single lab's unilateral caution would prevent catastrophic outcomes — some baseline of shared technical safeguards against loss of control is a real public good.
% TRANSFER_FUNCTION: Moves research funding, policy attention, regulatory capacity, and public salience away from auditing and remediating present-day deployed-system harms and toward speculative long-horizon catastrophic scenarios, benefiting institutions whose funding and legitimacy are tied to the catastrophic framing.
% ABSENT_VOICES: Communities currently harmed by deployed algorithmic systems, and Global South populations subject to systems safety-tested against Western catastrophic benchmarks, are largely absent from the institutes and labs setting the alignment research agenda; their objection would be that 'alignment' has been defined to exclude the harms they experience today.
% DISAPPEARANCE_RATIONALE: If the catastrophic-control framing vanished overnight, frontier labs would lose a central legitimating narrative for self-regulation and scale, and existential-risk institutes would lose their primary funding rationale — the world clearly rearranges for those seats. Whether the underlying catastrophic risk itself would rearrange (i.e., whether the risk is real and reduced by current work) is exactly the disputed empirical question the reading cannot settle from inside itself.
% FOUNDING_PROBLEM: As AI systems approached and then exceeded human performance on economically significant tasks with increasing autonomy, researchers argued that a sufficiently capable, goal-directed system could resist correction or shutdown by its operators, making loss of meaningful human control an unrecoverable failure mode distinct from ordinary software defects.
% FOUNDING_PROBLEM_CORROBORATION: Some independent ML researchers outside the major labs and existential-risk institutes (e.g., academic critics of 'AI safety' funding concentration) attest that catastrophic loss-of-control scenarios remain speculative relative to demonstrated present-day harms, and argue the framing serves lab legitimacy and funding capture more than it serves any constituency currently experiencing algorithmic harm. No corroboration from outside the beneficiary set currently affirms that the catastrophic scenario is imminent enough to justify its share of the field's resources; this absence of independent corroboration is itself part of the contested record.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__safety_control_reading, contested).
narrative_ontology:founding_problem_status(ai_alignment_commitment__safety_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__safety_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_commitment__safety_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__safety_control_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__safety_control_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__safety_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__safety_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects a real but partial transfer: catastrophic-control framing captures a disproportionate share of funding, policy salience, and research prestige relative to its demonstrated present harm-reduction value, at the expense of present-day harm mitigation work. It is not maximal because the underlying coordination problem (preventing loss of control) is not fabricated — some genuine technical safety work would need to exist under any reading. Suppression (0.48) is moderate: there is no legal bar on pursuing present-harm research, but institutional gatekeeping (grant panels, publication venues, lab hiring priorities) makes the catastrophic framing structurally easier to fund and publish, and this pressure has intensified over the interval as the framing became dominant in major labs' public safety commitments. Theater ratio (0.40) captures that a meaningful share of 'safety team' activity under this framing is now oriented toward public legitimacy signaling (safety pledges, voluntary commitments) rather than technical work with measurable present-day risk reduction.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontier labs and existential-risk institutes sit near the beneficiary end: they set the agenda, capture funding and legitimacy, and have mobile/arbitrage exit (labs can reallocate safety-team framing at will; institutes can pivot funding narratives). Present-day-harm communities and Global South deployment populations sit near the full-target end: they are trapped or constrained in exit, bear the cost of diverted attention, and have no seat in agenda-setting. Present-harm researchers sit as moderate-power targets: they can seek alternative funding but compete on unequal terms against the dominant framing. Future generations are named beneficiaries but are a non-agent seat — their actual benefit is conditional on the catastrophic scenario being the real threat, which is exactly the omega this story cannot resolve from inside the reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (loss-of-control risk from increasingly autonomous, capable systems) is genuinely live in a technical sense — it has not been solved or disproven. What makes this tangled rather than a clean rope is that the SHARE of institutional resources and legitimacy the framing commands has grown disproportionately to any resolution of the underlying uncertainty, and grown specifically in ways that serve the agenda-setting labs' self-regulatory position. Classifying this as tangled_rope (not snare) preserves the genuine coordination function against those who would dismiss all alignment work as pure capture, while the beneficiary/victim/enforcement structure prevents the opposite error of treating the framing's dominance as costless or purely benign.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophic_probability_calibration,
    'Is the probability and severity of catastrophic loss-of-control scenarios high enough to justify the share of resources, funding, and policy attention the safety-control reading currently commands, relative to demonstrated present-day algorithmic harms?',
    'Long-horizon tracking of whether frontier systems exhibit early loss-of-control precursor behaviors (goal misgeneralization, deceptive alignment, resistance to correction) at rates and severities consistent with the catastrophic framing''s implicit risk estimates, compared against documented, quantified present-day harm rates from deployed systems over the same interval.',
    'If catastrophic precursors fail to materialize at meaningful rates while present-day harms continue accumulating and under-remediated, this reading''s extractiveness score should be revised upward and its coordination-function claim weakened toward snare; if precursors do materialize, the coordination function is vindicated and extraction is better read as necessary cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophic_probability_calibration, empirical, 'Whether catastrophic risk severity justifies the reading''s resource claim relative to present harms.').

omega_variable(
    committer_kernel_disagreement_location,
    'This story is one reading (safety_control_reading) of the ai_alignment_commitment kernel; the sibling readings are ethics_justice_reading (present-day bias/harm) and integrated_reading (simultaneous, non-exclusive attention to both). Where exactly is the disagreement located: is it about which harms are worse, about who should be included in the victim set, or about whether the two harm classes compete for the same finite resource pool at all?',
    'Structural analysis of whether alignment funding, researcher time, and policy attention are genuinely fungible across catastrophic-risk and present-harm work (a zero-sum resource pool) or whether the appearance of competition is itself an artifact of institutional framing choices that could be reorganized to fund both without tradeoff.',
    'If the resource pools are genuinely zero-sum, this reading''s high extractiveness from present-harm mitigation is structurally forced by its dominance and the integrated_reading is the only non-extractive resolution. If the pools are not genuinely zero-sum (separable funding streams exist), the appearance of competition is itself part of what should be classified as extractive institutional behavior, not a real tradeoff.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_disagreement_location, conceptual, 'Locates where the safety_control and ethics_justice readings actually diverge: resource competition, victim-set inclusion, or genuine substantive priority.').

omega_variable(
    future_generations_non_agency,
    'Future generations are declared a beneficiary but are a non-agent entity with no capacity to corroborate whether the catastrophic-control framing actually serves their interests, or whether resources spent on speculative catastrophic scenarios would have better served them if redirected toward present-day harm remediation that compounds into their inherited world.',
    'No direct resolution is possible (non-agent beneficiaries cannot self-report); proxy resolution via long-run tracking of which harm class (catastrophic AI failure vs. accumulated algorithmic discrimination and displacement) produces greater aggregate harm to future populations, assessed retrospectively by historians and economists decades hence.',
    'If present-day harms compound more severely into the future than catastrophic risk materializes, the beneficiary framing for future generations under this reading is misattributed — they would have been better served by the ethics_justice_reading''s resource allocation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generations_non_agency, conceptual, 'Whether naming future generations as beneficiaries of the catastrophic-control reading is empirically defensible or a rhetorical move with no corroborating agent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__safety_control_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__safety_control_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(ai_a_tr_t0, observed).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_commitment__safety_control_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement_basis(ai_a_tr_t4, observed).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_commitment__safety_control_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(ai_a_tr_t8, observed).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_commitment__safety_control_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement_basis(ai_a_tr_t12, observed).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_commitment__safety_control_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement_basis(ai_a_tr_t16, observed).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__safety_control_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(ai_a_tr_t20, projected).
narrative_ontology:measurement(ai_a_tr_t24, ai_alignment_commitment__safety_control_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(ai_a_tr_t24, projected).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__safety_control_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(ai_a_be_t0, observed).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_commitment__safety_control_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement_basis(ai_a_be_t4, observed).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_commitment__safety_control_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(ai_a_be_t8, observed).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_commitment__safety_control_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement_basis(ai_a_be_t12, observed).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_commitment__safety_control_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(ai_a_be_t16, observed).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__safety_control_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(ai_a_be_t20, projected).
narrative_ontology:measurement(ai_a_be_t24, ai_alignment_commitment__safety_control_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(ai_a_be_t24, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__safety_control_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(ai_a_su_t0, observed).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_commitment__safety_control_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement_basis(ai_a_su_t4, observed).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_commitment__safety_control_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement_basis(ai_a_su_t8, observed).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_commitment__safety_control_reading, suppression_requirement, 12, 0.43).
narrative_ontology:measurement_basis(ai_a_su_t12, observed).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_commitment__safety_control_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement_basis(ai_a_su_t16, observed).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_commitment__safety_control_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement_basis(ai_a_su_t20, projected).
narrative_ontology:measurement(ai_a_su_t24, ai_alignment_commitment__safety_control_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement_basis(ai_a_su_t24, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__safety_control_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__safety_control_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment__ethics_justice_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment__integrated_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the ai_alignment_commitment kernel. ethics_justice_reading authors a different ε and victim set centered on present-day bias/harm communities as primary victims and treats catastrophic-risk framing as itself extractive of attention. integrated_reading treats the two harm classes as non-exclusive and authors a lower extractiveness score reflecting a coordination structure that funds both without forced tradeoff. Each reading is ε-invariant on its own terms; they are linked here rather than merged because measuring 'alignment' by catastrophic-risk-reduction versus present-harm-reduction yields structurally different ε values — the hallmark of needing separate constraint stories per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_commitment__safety_control_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
