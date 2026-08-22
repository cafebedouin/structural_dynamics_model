% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: AI Alignment as Existential Risk Prevention
 *   domain: technology/governance/risk assessment
 *
 * SUMMARY:
 *   The existential-risk reading of AI alignment frames the core problem as
 *   preventing catastrophic loss of control over future advanced AI
 *   systems—treating this as the binding constraint on safety research,
 *   policy, and resource allocation. This reading instantiates a specific
 *   constraint story: it identifies beneficiaries (capability research
 *   institutions, future generations), victims (present marginalized
 *   populations, near-term safety researchers), and a particular causal
 *   structure (capability understanding → future safety). The reading claims
 *   high extractiveness because it systematically redirects resources away
 *   from present harms and near-term governance, creating a resource-transfer
 *   mechanism from those harmed today to speculative future scenarios. It is
 *   a tangled rope: genuine coordination (unified research framing) coupled
 *   with asymmetric extraction (resource reallocation favoring
 *   capability-focused institutions over present-harm mitigation). The
 *   measurement series tracks the rising theater ratio (increasing proportion
 *   of alignment activity devoted to narrative/legitimacy work vs. technical
 *   safety advances) and the rising suppression requirement (effort needed to
 *   maintain the existential framing against competing near-term framings).
 *
 * KEY AGENTS:
 *   - existential_risk_researchers: institutional beneficiary and agenda-setter; frames future risk as primary
 *   - capability_research_labs: institutional beneficiary; receives resource flows justified by existential framing
 *   - present_marginalized_populations: powerless payers; bear harms from deployed systems; excluded from alignment agenda
 *   - near_term_safety_researchers: moderate-power payers; receive less funding and legitimacy than existential-risk teams
 *   - deployed_ai_systems_operators: powerful beneficiaries; use existential framing to defer present accountability
 *   - policy_makers: institutional agenda-setters; navigate framing contest and allocate legitimacy to frameworks
 *   - global_south_populations: powerless payers; treated as part of undifferentiated future beneficiary; present governance excluded
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, 0.68).
domain_priors:suppression_score(ai_alignment_priority__existential_risk_reading, 0.62).
domain_priors:theater_ratio(ai_alignment_priority__existential_risk_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__existential_risk_reading, "AI Alignment as Existential Risk Prevention").
narrative_ontology:topic_domain(ai_alignment_priority__existential_risk_reading, "technology/governance/risk assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__existential_risk_reading, '4259443c-1926-4b19-b3df-ab5e69b73f8c').
narrative_ontology:cs_kernel_codification('4259443c-1926-4b19-b3df-ab5e69b73f8c', distributed).
narrative_ontology:cs_authority_grounding('4259443c-1926-4b19-b3df-ab5e69b73f8c', extraction).
narrative_ontology:cs_interpretation_layer_present('4259443c-1926-4b19-b3df-ab5e69b73f8c').
narrative_ontology:cs_reading_relation('4259443c-1926-4b19-b3df-ab5e69b73f8c', ai_alignment_priority__nearterm_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('4259443c-1926-4b19-b3df-ab5e69b73f8c', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('4259443c-1926-4b19-b3df-ab5e69b73f8c', foundational, catastrophic_capability_loss_of_control_is_binding_constraint).
narrative_ontology:cs_axiom_status(catastrophic_capability_loss_of_control_is_binding_constraint, holdable).
narrative_ontology:cs_axiom_grounding('4259443c-1926-4b19-b3df-ab5e69b73f8c', catastrophic_capability_loss_of_control_is_binding_constraint, empirically_contingent).
narrative_ontology:cs_axiom('4259443c-1926-4b19-b3df-ab5e69b73f8c', foundational, present_harms_are_secondary_to_speculative_extinction_risk).
narrative_ontology:cs_axiom_status(present_harms_are_secondary_to_speculative_extinction_risk, holdable).
narrative_ontology:cs_axiom_grounding('4259443c-1926-4b19-b3df-ab5e69b73f8c', present_harms_are_secondary_to_speculative_extinction_risk, deontological).
narrative_ontology:cs_reference_frame('4259443c-1926-4b19-b3df-ab5e69b73f8c', capability_understanding_as_safety_foundation).
narrative_ontology:cs_drift_state('4259443c-1926-4b19-b3df-ab5e69b73f8c', present_materialized_ai_harms_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4259443c-1926-4b19-b3df-ab5e69b73f8c', '2026-06-12T14:23:15Z').
narrative_ontology:cs_kernel_id(ai_alignment_priority__existential_risk_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, future_generations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, capability_research_institutions).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, resource_constrained_safety_research).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, capability_research_labs).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, deployed_ai_systems_operators).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, commercial_ai_developers).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, near_term_safety_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, deployed_ai_systems_operators).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, global_south_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define alignment as preventing catastrophic loss of control over future advanced AI; conduct adversarial red-teaming and capability research to understand failure modes; secure research funding and institutional legitimacy by framing the problem as an extinction-level threat. Their research agendas are shaped by the assumption that understanding capability ceilings is the bottleneck to safety.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, existential_risk_researchers, agenda_setter,
    institutional, civilizational, constrained, global).

% Receive research grants, talent, and legitimacy framed around understanding and testing AI capabilities. The existential framing positions capability research as foundational to safety, securing resources that might otherwise flow to near-term safety work or other domains. Their incentive structure aligns capability advancement with safety framing.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, capability_research_labs, beneficiary,
    institutional, generational, arbitrage, global).

% Non-agent placeholder: those who will inherit whatever AI governance landscape is built today. Cannot participate in present decisions; represented only through advocates who claim to speak for their interests.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_alignment_priority__existential_risk_reading, future_generations).

% Bear harms from deployed discriminatory AI systems today—biased hiring algorithms, predatory lending tools, surveillance systems targeting minorities. Resources flowing to existential-risk framing are resources unavailable to address these present harms. Their exclusion from the alignment agenda is structural: the existential framing treats present deployment harms as secondary to speculative future risks.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, present_marginalized_populations, payer,
    powerless, biographical, trapped, global).

% Work on interpretability, bias mitigation, fair deployment, and governance of currently-deployed systems. Receive less funding and institutional legitimacy than existential-risk teams. Their research is often framed as orthogonal or subordinate to capability understanding, even though present harms are already materializing. Excluded from agenda-setting conversations about what 'alignment' means.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, near_term_safety_researchers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, near_term_safety_researchers, excluded).

% Benefit from the existential framing because it shifts accountability away from present deployment decisions (framed as minor vs. catastrophic future risk) and toward foundational research agendas they do not directly control. Also bear some compliance costs with emerging alignment regulations and red-teaming requirements, making the relationship asymmetric.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, deployed_ai_systems_operators, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, deployed_ai_systems_operators, payer).

% Navigate competing framings of alignment and set research priorities, regulatory requirements, and resource allocation. The existential framing creates a legitimacy cascade: prioritizing existential risk allows governments to claim alignment with the 'most serious' problem and to anchor policy on capability control rather than present deployment governance.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, policy_makers_and_governments, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, policy_makers_and_governments, observer).

% Benefit from an alignment framing centered on capability research and future risk, because it licenses current capability expansion as part of 'responsible research' and defers present-deployment accountability to future safety measures. Can engage with existential-risk framing as a legitimacy cover while maintaining capability development velocity.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, commercial_ai_developers, beneficiary,
    powerful, biographical, arbitrage, global).

% Face harms from deployed AI systems (biased medical diagnosis, extractive labor matching) without representation in either existential-risk or near-term-safety agendas. The existential framing treats them as part of undifferentiated 'all humanity' future beneficiary, obscuring their present exclusion from governance decisions and present harms from systems built without their input.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, global_south_populations, payer,
    powerless, biographical, trapped, global).

% Diverse field spanning existential-risk, near-term-safety, and integrated perspectives. Acts as a distributed observer of the constraint; different seats within the community adopt different readings and contest the agenda-setting framing.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, alignment_research_community, observer,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__existential_risk_reading, capability_research_labs).
narrative_ontology:fixing_cost_class(ai_alignment_priority__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates research effort, funding allocation, and technical priority-setting around a unified understanding of what 'alignment' fundamentally means—a shared frame that enables otherwise-fragmented researchers and institutions to recognize their work as contributing to a coherent goal. Solves the collective-action problem of distributed safety research having no single success criterion.
% TRANSFER_FUNCTION: Transfers research resources (grants, talent, institutional status) from near-term safety and deployment governance domains to capability-focused existential-risk research. Transfers attention (policy, media, academic focus) away from present harms toward speculative future risks. Transfers legitimacy to institutions and researchers who adopt the existential framing.
% ABSENT_VOICES: Marginalized populations bearing present AI harms have no seat at alignment agenda-setting; workers displaced by AI systems are excluded; affected communities in the Global South are absent; near-term safety researchers and their research communities are structurally sidelined. The existential framing claims to represent 'all humanity' future but actively excludes present voices from governance.
% DISAPPEARANCE_RATIONALE: If the existential-risk framing of alignment disappeared overnight, capability research would continue but under different legitimacy claims; near-term safety and deployment governance would receive more resources and policy focus; present harms would move from 'secondary' to primary concern in alignment discussions. The world would not rearrange—capability research would persist—but the governance structure and resource allocation would shift sharply. The contest is whether the present world depends on this framing (beneficiaries claim yes; payers claim the dependence is constructed).
% FOUNDING_PROBLEM: Advanced AI systems could, in principle, become sufficiently capable and misaligned that human control becomes impossible, leading to outcomes catastrophic for humanity. The founding problem assumes: (1) capabilities will eventually exceed human ability to verify safety; (2) alignment failures at scale would be globally catastrophic; (3) understanding the shape of failure modes now can prevent them later.
% FOUNDING_PROBLEM_CORROBORATION: Existential-risk researchers and some AI capability researchers attest the founding problem is live and urgent. Near-term safety researchers, marginalized-population advocates, and deployment-governance experts dispute whether the founding problem is the binding constraint (they attest present harms are materializing now and the existential framing defers accountability). Policy makers and commercial developers accept the framing instrumentally. No corroboration from outside the existential-risk institutional complex itself.
narrative_ontology:disappearance_verdict(ai_alignment_priority__existential_risk_reading, contested).
narrative_ontology:founding_problem_status(ai_alignment_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__existential_risk_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The extractiveness score (0.68 at interval end) reflects a substantial transfer of resources from present-harm mitigation to future-risk speculation, justified by the existential framing. The transfer is structured asymmetrically: beneficiaries (capability labs, existential researchers) secure resources and legitimacy; payers (marginalized populations, near-term researchers, displaced workers) lose resources and policy attention. Suppression (0.62) measures the effort required to maintain the existential framing against alternative definitions of alignment—the need to actively exclude or downgrade near-term and present-harm framings. Theater ratio (0.41) reflects that a growing share of alignment activity is devoted to legitimacy work (media, policy narrative, institutional positioning) rather than technical safety research or deployment governance. The rising trajectory on all three metrics suggests the framing has matured and institutionalized, requiring more theater to maintain and suppressing more alternative voices. Accessibility collapse (0.58) is moderate because alternative framings are still live and articulate—near-term safety research continues, marginalized-population advocates speak to the present harms—but the existential framing has captured dominant institutional and policy authority. Resistance (0.72) is high because near-term researchers, deployment-governance advocates, and affected communities actively contest the framing; the constraint persists despite substantial resistance.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (existential researchers, policy makers) experience this as genuinely necessary coordination—a shared frame enabling fragmented research to converge on a unified safety goal. From their position, the extractiveness is an unfortunate but necessary reallocation of finite resources toward the most critical problem. The payer seats (marginalized populations, near-term researchers) experience the same structure as enforced extraction—their present harms are systematically deprioritized, their voices excluded, their research de-legitimized. The engine should compute this divergence: an institutional agenda-setter with arbitrage exit will experience low directionality (strong beneficiary position); a powerless, trapped payer will experience high directionality (strong target position). The same constraint measures as fundamentally different from these two seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Existential-risk researchers and capability labs: institutional power, arbitrage exit (could shift to other research domains), beneficiary role → low d (near beneficiary end). Marginalized populations and near-term researchers: powerless/moderate power, trapped or constrained exit, payer role → high d (near target end). Future generations: non-agent placeholder with universal scope and civilizational horizon, beneficiary role but no agency → computational artifact requiring omega treatment. Policy makers: institutional power, mobile exit (can shift between frameworks), dual agenda-setter/observer role → middle d with potential for override if the framing contest shifts.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows mandatrophy symptoms but not full mandatrophy: the founding problem (catastrophic misalignment from advanced AI) was genuinely urgent when the framing emerged, but the framing has matured into institutional inertia. Present harms from deployed AI are materializing (bias, discrimination, extraction) while speculative future risks remain speculative. The constraint is classified as tangled rope rather than snare because a genuine coordination function (unified safety research frame) persists alongside extraction. However, the rising theater ratio and stable-then-declining suppression suggest the coordination function may be atrophying—much of the current activity is maintaining the narrative against competing framings rather than advancing technical understanding. The contest between existential and near-term readings will clarify whether the founding problem (preventing catastrophic future misalignment) is still binding or has been superseded by present-harm prevention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capability_ceiling_epistemic_gap,
    'Will advanced AI systems actually reach levels of capability that humans cannot verify or control? Or will the capability ceiling remain within verifiable and controllable bounds?',
    'Empirical: continued development of AI systems and observation of whether they exhibit unexpected capabilities, adversarial robustness beyond design specifications, or emergent behaviors. The resolution requires either crossing the threshold (confirming the existential risk) or reaching a capability plateau below it.',
    'If capability ceiling is lower than existential-risk researchers project, the founding problem is less binding than claimed and resource reallocation away from near-term harms is harder to justify. If ceiling is exceeded, the foundational claim validates and the constraint''s resource priorities are vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capability_ceiling_epistemic_gap, empirical, 'Whether future AI capabilities will exceed human control bounds.').

omega_variable(
    present_vs_speculative_prioritization,
    'Should research and governance priorities address present, materializing harms (discriminatory deployment, extractive systems) or speculative future risks? Are these framings competing or complementary?',
    'Policy and institutional choice: jurisdictions can choose to prioritize present-harm mitigation, future-risk prevention, or integrated approaches. The resolution is partially preference-based (values-determined) and partially empirical (which approach reduces harm more effectively).',
    'If present harms are prioritized, resources flow to near-term safety research, deployment governance, and accountability mechanisms; the existential framing recedes. If complementarity is established, both framings gain legitimacy and resource sharing becomes the question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(present_vs_speculative_prioritization, preference, 'The fundamental contest between existential and present-harm framings.').

omega_variable(
    future_generations_representation,
    'Who legitimately speaks for future generations? Can present agents authentically represent the interests of those not yet born, or is the representation necessarily a projection of present values?',
    'Conceptual and procedural: institutions can establish representation mechanisms (future-generation ombudspersons, multi-generational governance structures, long-term impact assessments). But the core problem—whether future-generations advocacy can be anything but present actors'' constructed preferences—is not empirically resolvable.',
    'If future-generation representation is seen as legitimate and authentic, the existential framing''s claim to represent them holds. If future-generation claims are recognized as present-value projections, the framing loses its moral authority and becomes one perspective among others rather than a universal humanitarian priority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(future_generations_representation, conceptual, 'Whether present agents can legitimately represent future interests, or whether the claim is inherently contestable.').

omega_variable(
    kernel_contest_reading_decomposition,
    'Is the ai_alignment_priority kernel a single contested concept (with three readings of one problem), or are existential_risk, near_term_harms, and integrated actually three different problems misnamed as versions of alignment?',
    'Structural: if the three readings share core causal mechanisms, victim sets, and beneficiary structures, they are readings of one kernel. If they diverge fundamentally on what ''alignment'' means—different referents, different causal chains—they are separate constraints that the kernel label obscures.',
    'If three different constraints: each gets its own ε, its own type, its own story. The existential reading becomes a pure constraint story alongside two others, ending the forced contest. If one kernel: the readings remain in tension, the kernel remains contested, and integrating perspectives becomes a governance challenge rather than a classification problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_reading_decomposition, conceptual, 'Whether the kernel contest reflects genuine framing disagreement (readings) or different problem definitions misnamed as alignment.').

omega_variable(
    adversarial_red_teaming_extractiveness,
    'Does adversarial red-teaming and capability research accelerate or decelerate present harms? Is capability exploration a genuine safety input, or a mechanism that amplifies harms by advancing capability beyond safety understanding?',
    'Empirical and causal: track deployed-system harm rates before/after capability research expansions; measure whether capability understanding precedes or lags harm emergence in practice.',
    'If capability research accelerates harms, the existential framing''s resource-transfer mechanism is directly harmful in the present. If capability research genuinely improves safety trajectory, the resource transfer is justified as net-positive future-orientation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adversarial_red_teaming_extractiveness, empirical, 'Whether advancing AI capabilities reduces or amplifies present harms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__existential_risk_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__existential_risk_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ai_a_tr_t3, ai_alignment_priority__existential_risk_reading, theater_ratio, 3, 0.31).
narrative_ontology:measurement(ai_a_tr_t6, ai_alignment_priority__existential_risk_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_priority__existential_risk_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement(ai_a_tr_t18, ai_alignment_priority__existential_risk_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement(ai_a_tr_t25, ai_alignment_priority__existential_risk_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__existential_risk_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(ai_a_be_t3, ai_alignment_priority__existential_risk_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(ai_a_be_t6, ai_alignment_priority__existential_risk_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_priority__existential_risk_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(ai_a_be_t18, ai_alignment_priority__existential_risk_reading, base_extractiveness, 18, 0.67).
narrative_ontology:measurement(ai_a_be_t25, ai_alignment_priority__existential_risk_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__existential_risk_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(ai_a_su_t3, ai_alignment_priority__existential_risk_reading, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(ai_a_su_t6, ai_alignment_priority__existential_risk_reading, suppression_requirement, 6, 0.56).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_priority__existential_risk_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(ai_a_su_t18, ai_alignment_priority__existential_risk_reading, suppression_requirement, 18, 0.62).
narrative_ontology:measurement(ai_a_su_t25, ai_alignment_priority__existential_risk_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__existential_risk_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__nearterm_harms_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__integrated_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_capability_governance).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_deployment_harm_mitigation).

% DUAL FORMULATION NOTE:
% This is one reading of the ai_alignment_priority kernel. The sibling readings (nearterm_harms_reading, integrated_reading) offer alternative framings of the same contested commitment (what alignment means). Each reading has a distinct ε, beneficiary/victim structure, and causal story. They are linked via network.affects_constraints because they are structurally interdependent: if one reading dominates governance, the others are marginalized or foreclosed. All three stories together instantiate the kernel contest; individual stories are best understood as one seat in a multi-reading ecosystem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_priority__existential_risk_reading, powerless, 0.88).
constraint_indexing:directionality_override(ai_alignment_priority__existential_risk_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
