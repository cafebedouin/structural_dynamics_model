% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__existential_risk_reading, []).

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
 *   constraint_id: ai_alignment_priority__existential_risk_reading
 *   human_readable: Existential Risk Priority Reading of AI Alignment
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   This constraint story models the 'existential risk reading' of the
 *   contested AI alignment priority kernel. In this reading, alignment is
 *   defined narrowly as preventing catastrophic loss of control over future
 *   advanced AI systems, with existential safety accorded overriding
 *   priority. The constraint operates as a resource-allocation and discursive
 *   regime: it coordinates the global AI safety community around adversarial
 *   red-teaming and capability evaluation while extracting funding, talent,
 *   and legitimacy from near-term harms research and justice-oriented AI
 *   ethics. The beneficiary set comprises frontier AI labs, existential risk
 *   research institutes, and longtermist grantmakers who capture the resource
 *   flow; the payer set includes near-term harms researchers and marginalized
 *   communities experiencing present AI-driven extraction whose concerns are
 *   systematically deprioritized. The reading presents itself as natural and
 *   urgent (a survival imperative) but exhibits high extractiveness and
 *   requires active enforcement through funding gatekeeping, conference
 *   boundary-policing, and methodological monopoly.
 *
 * KEY AGENTS:
 *   - frontier_ai_labs: Primary agenda-setter and beneficiary (institutional/arbitrage) â controls capability research and safety framing
 *   - existential_risk_research_institutes: Primary beneficiary (organized/constrained) â receives funding and prestige under the reading
 *   - longtermist_grantmakers: Agenda-setter (powerful/mobile) â enforces priority through resource allocation
 *   - nearterm_harms_researchers: Primary payer (moderate/constrained) â loses funding and legitimacy
 *   - marginalized_communities_affected_by_ai: Primary payer (powerless/trapped) â bears present harms deprioritized by the constraint
 *   - ai_ethics_and_policy_community: Excluded voice (organized/constrained) â structurally marginalized from alignment discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, 0.82).
domain_priors:suppression_score(ai_alignment_priority__existential_risk_reading, 0.75).
domain_priors:theater_ratio(ai_alignment_priority__existential_risk_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__existential_risk_reading, "Existential Risk Priority Reading of AI Alignment").
narrative_ontology:topic_domain(ai_alignment_priority__existential_risk_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__existential_risk_reading, 'a930730b-bf48-4374-a220-c4193d4671f5').
narrative_ontology:cs_kernel_codification('a930730b-bf48-4374-a220-c4193d4671f5', distributed).
narrative_ontology:cs_authority_grounding('a930730b-bf48-4374-a220-c4193d4671f5', expertise).
narrative_ontology:cs_interpretation_layer_present('a930730b-bf48-4374-a220-c4193d4671f5').
narrative_ontology:cs_reading_relation('a930730b-bf48-4374-a220-c4193d4671f5', ai_alignment_priority__nearterm_harms_reading, influences).
narrative_ontology:cs_reading_relation('a930730b-bf48-4374-a220-c4193d4671f5', ai_alignment_priority__integrated_reading, coexists_with).
narrative_ontology:cs_axiom('a930730b-bf48-4374-a220-c4193d4671f5', foundational, existential_safety_lexical_priority).
narrative_ontology:cs_axiom_status(existential_safety_lexical_priority, holdable).
narrative_ontology:cs_axiom_grounding('a930730b-bf48-4374-a220-c4193d4671f5', existential_safety_lexical_priority, instrumental).
narrative_ontology:cs_axiom('a930730b-bf48-4374-a220-c4193d4671f5', foundational, adversarial_redteaming_primary_methodology).
narrative_ontology:cs_axiom_status(adversarial_redteaming_primary_methodology, holdable).
narrative_ontology:cs_axiom_grounding('a930730b-bf48-4374-a220-c4193d4671f5', adversarial_redteaming_primary_methodology, instrumental).
narrative_ontology:cs_reference_frame('a930730b-bf48-4374-a220-c4193d4671f5', classical_xrisk_urgency_framework).
narrative_ontology:cs_drift_state('a930730b-bf48-4374-a220-c4193d4671f5', contemporary_ai_governance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a930730b-bf48-4374-a220-c4193d4671f5', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__existential_risk_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, existential_risk_research_institutes).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, longtermist_grantmakers).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, nearterm_harms_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, marginalized_communities_affected_by_ai).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and deploy frontier AI models while framing capability scaling as necessary for existential safety research. They set the technical agenda for what counts as alignment work, attract safety-oriented talent and funding, and benefit from the legitimacy of the existential-risk framing which justifies continued scaling and concentration of resources.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, frontier_ai_labs, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, frontier_ai_labs, beneficiary).

% Receive dedicated funding and institutional prestige by defining alignment as existential risk mitigation. They produce the adversarial red-teaming methodologies and theoretical frameworks that legitimize the priority framing. Their careers and organizational survival depend on maintaining the salience of speculative catastrophic scenarios.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, existential_risk_research_institutes, beneficiary,
    organized, civilizational, constrained, global).

% Allocate large pools of philanthropic capital toward capability-focused safety research and away from near-term harm mitigation. They shape the incentive landscape by defining grant eligibility around existential risk criteria, effectively enforcing the priority reading through resource allocation.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, longtermist_grantmakers, agenda_setter,
    powerful, civilizational, mobile, global).

% Study present discriminatory and extractive impacts of deployed AI systems. Under the existential-risk priority framing, their work is delegitimized as not real alignment, their funding pools shrink, and they face pressure to reframe their research in catastrophic-risk terms to access safety funding.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, nearterm_harms_researchers, payer,
    moderate, biographical, constrained, national).

% Experience present-day harms from AI systems such as biased criminal justice risk scores, exploitative labor practices in data labeling, and loss of economic opportunity. The existential-risk priority framing diverts policy attention and technical resources from mitigating these harms, treating them as negligible compared to speculative future catastrophic scenarios.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, marginalized_communities_affected_by_ai, payer,
    powerless, immediate, trapped, global).

% Advocates for justice-centered, present-focused AI governance approaches. They are structurally excluded from high-status alignment discourse, conference keynotes, and major funding streams when the existential-risk reading dominates the definition of the field, despite possessing relevant technical and social expertise.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, ai_ethics_and_policy_community, excluded,
    organized, biographical, constrained, global).

% Conduct third-party audits of AI systems without being captured by either the capability research ecosystem or the ethics community. They observe that the adversarial red-teaming methodology often produces performative safety demonstrations rather than structural risk reduction, but their findings are absorbed into the dominant framing without shifting resource allocation.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, independent_safety_evaluators, observer,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__existential_risk_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preventing catastrophic loss of control over advanced AI systems by coordinating global research priorities, standards, and funding around existential risk mitigation and adversarial evaluation.
% TRANSFER_FUNCTION: Moves research funding, talent, and institutional legitimacy from near-term harm mitigation and justice-oriented AI ethics toward capability-focused safety research and frontier model evaluation.
% ABSENT_VOICES: Researchers and communities focused on present discriminatory and extractive harms from deployed AI systems, who are delegitimized as not doing real alignment work, and whose funding and access are correspondingly reduced.
% DISAPPEARANCE_RATIONALE: If the existential-risk-first framing vanished, near-term harms research would regain parity in funding and prestige, frontier labs would face diversified accountability pressures, and the adversarial red-team methodology would lose its monopoly on alignment legitimacy â the field would reorganize around integrated or justice-centered framings.
% FOUNDING_PROBLEM: The prospect of advanced AI systems developing capabilities that exceed human control, leading to catastrophic or existential outcomes, with no existing governance or technical framework adequate to prevent such loss of control.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by existential risk researchers and some frontier lab safety teams. It is contested by near-term harms researchers, social scientists, and independent technology policy analysts who argue the problem is either ungrounded in present evidence or is a distraction from documented current harms. No corroboration from outside the benefiting parties is unanimous; the contestation itself is the signal.
narrative_ontology:disappearance_verdict(ai_alignment_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__existential_risk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__existential_risk_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the priority framing diverts substantial resources from present harms to speculative capability research with decoupled accountability. Suppression (0.75) reflects active enforcement: alternative definitions of alignment are delegitimized in major venues, and funding is explicitly gated on existential-risk relevance. Theater_ratio (0.50 at interval end) captures the growing share of adversarial evaluation that produces demonstrable but narrow safety theater rather than structural risk reduction. Accessibility_collapse (0.60) registers that while alternative approaches exist, they lose funding and status when the dominant framing is understood. Resistance (0.55) comes from the AI ethics community and some policy researchers who contest the narrow definition.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats compute the constraint as necessary coordination around a genuine catastrophic risk; from their perspective, the adversarial methodology and resource concentration are the price of survival. The payer and excluded seats compute it as extraction: a definitional capture of the term 'alignment' that starves alternative safety work and present-focused accountability. The engine computes this divergence from identical structural data because directionalities and scopes differ across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontier AI labs and existential risk institutes are structural beneficiaries: they receive the diverted resources and legitimacy, sit near the low-d (beneficiary) end. Longtermist grantmakers are agenda-setters who administer the priority; their exit is mobile but their directionality is low because they control the constraint rather than paying its costs. Near-term harms researchers and marginalized communities are structural targets: they bear the costs of deprioritization and delegitimization, sit near the high-d (target) end. The ethics community is excluded from the conversation entirely, experiencing the constraint as absolute access denial rather than cost-bearing.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents the false binary of labeling this either pure coordination (rope) or pure extraction (snare). There is a genuine collective-action problem â unaligned advanced AI could indeed pose catastrophic risk â which satisfies the coordination test. However, the same structural apparatus that coordinates safety research also concentrates resources in capability-focused actors and suppresses integrated or justice-centered approaches, satisfying the asymmetric extraction test. The mandatrophy flag would fire only if the founding problem (existential risk from loss of control) were resolved while the apparatus persisted; given the speculative nature of the risk, the founding problem status is contested rather than dead, so the constraint is not yet a piton, though rising theater_ratio signals drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    xrisk_empirical_grounding,
    'Are the speculative capabilities that motivate existential-risk prioritization empirically likely within the time horizon that justifies present resource diversion?',
    'Track frontier AI capability trajectories against the predictions made by this reading''s proponents; if projected catastrophic capabilities fail to materialize or are shown to be contingent on specific paths that do not occur, the empirical premise weakens.',
    'If the empirical premise is weak or unfalsifiable, the constraint functions more as a snare â extraction without verifiable coordination function â and the high extractiveness is unearned. If strong, the coordination function is validated and tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(xrisk_empirical_grounding, empirical, 'Whether imminent catastrophic risk is empirically grounded or speculative').

omega_variable(
    alignment_boundary_work,
    'Does the adversarial red-teaming methodology represent the only viable path to safe AI, or does its dominance reflect institutional boundary-work that excludes legitimate alternatives?',
    'Comparative evaluation of safety outcomes between red-team-centric institutions and those adopting integrated or participatory methodologies; funding flow analysis showing whether alternative methodologies are starved or merely less effective.',
    'If red-teaming is genuinely superior, part of the measured extraction is the necessary cost of coordination. If dominance reflects boundary-work, the coordination story is cover and the constraint drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alignment_boundary_work, conceptual, 'Whether methodology monopoly is epistemic or extractive').

omega_variable(
    kernel_reading_ambiguity,
    'Is the existential-risk reading a legitimate interpretation of an under-specified alignment kernel, or has it effectively become a distinct constraint that captures the kernel label for extractive purposes?',
    'Historical corpus analysis of ''alignment'' usage: whether the term genuinely underdetermines between readings (supporting distributed kernel) or whether the existential reading has achieved definitional capture (supporting extraction interpretation).',
    'If the kernel is genuinely distributed, the reading is one live position among many. If capture has occurred, the constraint''s suppression score is higher than intra-kernel contestation would suggest, because dissent is framed as misunderstanding the term itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether this reading interprets or captures the alignment kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__existential_risk_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_align_xrisk_tr_t0, ai_alignment_priority__existential_risk_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_align_xrisk_tr_t5, ai_alignment_priority__existential_risk_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(ai_align_xrisk_tr_t10, ai_alignment_priority__existential_risk_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(ai_align_xrisk_tr_t15, ai_alignment_priority__existential_risk_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement(ai_align_xrisk_tr_t20, ai_alignment_priority__existential_risk_reading, theater_ratio, 20, 0.5).

% Extraction over time
narrative_ontology:measurement(ai_align_xrisk_be_t0, ai_alignment_priority__existential_risk_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(ai_align_xrisk_be_t5, ai_alignment_priority__existential_risk_reading, base_extractiveness, 5, 0.66).
narrative_ontology:measurement(ai_align_xrisk_be_t10, ai_alignment_priority__existential_risk_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(ai_align_xrisk_be_t15, ai_alignment_priority__existential_risk_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(ai_align_xrisk_be_t20, ai_alignment_priority__existential_risk_reading, base_extractiveness, 20, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(ai_align_xrisk_su_t0, ai_alignment_priority__existential_risk_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ai_align_xrisk_su_t5, ai_alignment_priority__existential_risk_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(ai_align_xrisk_su_t10, ai_alignment_priority__existential_risk_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(ai_align_xrisk_su_t15, ai_alignment_priority__existential_risk_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(ai_align_xrisk_su_t20, ai_alignment_priority__existential_risk_reading, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__existential_risk_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, nearterm_harms_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, integrated_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the contested 'ai_alignment_priority' kernel into three structurally distinct constraints per the epsilon-invariance principle: the existential-risk reading (high epsilon, capability-focused), the near-term harms reading (present-justice focused), and the integrated reading (complementary priorities). Each reading has distinct beneficiary/victim structures, metrics, and epsilon values. This file instantiates the existential-risk reading only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
