% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__existential_risk_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__existential_risk_reading
 *   human_readable: Existential Risk Prioritization in AI Governance (X-Risk Reading)
 *   domain: technology/governance/risk_assessment
 *
 * SUMMARY:
 *   The existential-risk reading of AI governance frames the dominant
 *   existential threat as misaligned superintelligence capable of
 *   irreversibly curtailing or eliminating humanity's future. It argues that
 *   governance resources, research prioritization, and policy constraints
 *   should be oriented toward preventing worst-case capability scenarios.
 *   This reading is authored as a tangled rope: it solves a real coordination
 *   problem (preventing a plausible failure mode of uncontrolled
 *   superintelligence) AND extracts resources and authority from alternative
 *   governance frameworks that prioritize present, measurable algorithmic
 *   harms. The beneficiaries are x-risk research institutions and labs
 *   positioning themselves as safety leaders; the victims are populations
 *   experiencing concrete AI harms now and future generations who cannot
 *   participate in present governance. The theater ratio rises as the
 *   governance infrastructure becomes increasingly performative—risk
 *   assessments focused on speculative capabilities while deployed systems
 *   continue causing concrete harm.
 *
 * KEY AGENTS:
 *   - x_risk_research_institutions: beneficiary (collects authority, funding, governance role)
 *   - ai_labs_claiming_safety_leadership: beneficiary (acquires regulatory deference through safety positioning)
 *   - present_harm_affected_populations: victim (governance resources diverted from their concrete problems)
 *   - future_humanity: non-agent victim (invoked but cannot participate)
 *   - policy_makers_and_regulators: observer (choose allocation between competing governance priorities)
 *   - global_south_populations: excluded (affected by both present harms and governance decisions but not seated)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, 0.68).
domain_priors:suppression_score(ai_risk_governance_priority__existential_risk_reading, 0.61).
domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "Existential Risk Prioritization in AI Governance (X-Risk Reading)").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "technology/governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, '98c56a79-01a2-4ebe-8719-e46a28fa8152').
narrative_ontology:cs_kernel_codification('98c56a79-01a2-4ebe-8719-e46a28fa8152', distributed).
narrative_ontology:cs_authority_grounding('98c56a79-01a2-4ebe-8719-e46a28fa8152', extraction).
narrative_ontology:cs_interpretation_layer_present('98c56a79-01a2-4ebe-8719-e46a28fa8152').
narrative_ontology:cs_reading_relation('98c56a79-01a2-4ebe-8719-e46a28fa8152', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('98c56a79-01a2-4ebe-8719-e46a28fa8152', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('98c56a79-01a2-4ebe-8719-e46a28fa8152', foundational, superintelligence_existential_risk_supremacy).
narrative_ontology:cs_axiom_status(superintelligence_existential_risk_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('98c56a79-01a2-4ebe-8719-e46a28fa8152', superintelligence_existential_risk_supremacy, empirically_contingent).
narrative_ontology:cs_axiom('98c56a79-01a2-4ebe-8719-e46a28fa8152', secondary, governance_prioritization_zero_sum).
narrative_ontology:cs_axiom_status(governance_prioritization_zero_sum, holdable).
narrative_ontology:cs_axiom_grounding('98c56a79-01a2-4ebe-8719-e46a28fa8152', governance_prioritization_zero_sum, empirically_contingent).
narrative_ontology:cs_reference_frame('98c56a79-01a2-4ebe-8719-e46a28fa8152', capability_safety_gap_widening).
narrative_ontology:cs_drift_state('98c56a79-01a2-4ebe-8719-e46a28fa8152', contemporary_2025_capability_acceleration, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('98c56a79-01a2-4ebe-8719-e46a28fa8152', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, present_harm_affected_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, future_humanity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Non-profit and academic organizations (Future of Humanity Institute, Machine Intelligence Research Institute, Center for AI Safety, and alignment research teams within labs like Anthropic, DeepMind's safety divisions) that have built research programs, funding mechanisms, and institutional identities around x-risk from advanced AI. They author threat models, advise policymakers, publish alignment research, and set the research agenda for AI governance. They benefit materially from governance frameworks that channel resources toward existential-risk research and create regulatory leverage for safety-first development constraints. Their authority derives from claimed epistemic privileged access to long-tail AI capability scenarios and alignment theory. They face low exit pressure—abandoning x-risk framing would require dismantling their entire institutional structure.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions, agenda_setter).

% Frontier AI development labs (OpenAI, Anthropic, DeepMind, others) that have adopted safety-first public positioning and x-risk language in their governance statements and research agendas. They benefit from existential-risk prioritization because it allows them to justify proprietary control of model weights and training data as safety measures, to position internal safety testing as sufficient governance, and to acquire regulatory deference as 'safety leaders.' The x-risk frame also helps them recruit talent and attract venture capital by positioning themselves as taking civilizational stakes seriously. Their exit from the frame is constrained: dropping safety-first positioning would trigger regulatory scrutiny and reputational damage.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership, beneficiary,
    institutional, generational, constrained, global).

% Workers facing displacement from labor-substituting AI systems, racial and ethnic minorities subjected to algorithmic discrimination in hiring and lending, communities harmed by AI-generated misinformation and deepfakes, populations surveilled by AI-enabled security systems. They experience concrete, measurable harm from AI systems currently deployed. They have trapped exit: they cannot opt out of labor markets, lending systems, or judicial processes that deploy these systems. Their bargaining power is fragmented across geography and harm type. When governance resources are directed toward speculative x-risk scenarios, those same resources are NOT directed toward regulating present harm or providing redress to victims.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, present_harm_affected_populations, payer,
    powerless, immediate, trapped, global).

% The non-agent entity representing all future humans and their potential flourishing. This entity bears the existential consequences if superintelligent AI systems are deployed misaligned or with uncontrolled optimization objectives. It cannot participate in present governance conversations, cannot advocate for its interests, and cannot negotiate. It is invoked as the ultimate victim set by the existential-risk reading but has no seat at the table where governance prioritization is decided.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_governance_priority__existential_risk_reading, future_humanity).

% Teams and organizations whose primary focus is advancing AI capabilities (scaling architectures, expanding multimodal abilities, improving reasoning and planning, broadening tool use). They would argue that existential-risk prioritization imposes unnecessary delays and constraints that slow beneficial applications (scientific discovery, medical diagnosis, materials design). Their objections—that risks should be managed through deployment safeguards rather than capability restrictions, that near-term benefits outweigh speculative long-term risks—are formally excluded from governance conversations when x-risk prioritization is framed as non-negotiable civilizational imperative.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_capability_researchers, excluded,
    institutional, biographical, constrained, global).

% Government officials and regulatory bodies responsible for AI governance who must allocate policy attention and resources between addressing present, demonstrable harms and building safeguards for speculative future scenarios. They receive testimony and briefings from x-risk researchers, present-harm advocates, capability researchers, and affected communities. Their governance decisions (which harms to prioritize in regulation, which research to fund, which capabilities to restrict) determine whose interests materialize in law and which remain unaddressed.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, policy_makers_and_regulators, observer,
    institutional, generational, analytical, national).

% Communities in lower-income countries who face both present algorithmic harms (training-data labor exploitation without fair compensation, deployment of surveillance and biometric systems, extraction of data for training, exclusion from AI benefits) and who would bear existential-risk consequences. They are structurally excluded from governance conversations dominated by wealthy-country research institutions and US-headquartered AI labs. Their perspectives on how existential-risk governance frameworks distribute benefit and burden are not systematically solicited, and their labor and data are often conscripted to support both x-risk research infrastructure and present-harm-generating systems.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, global_south_populations, excluded,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates AI development governance around preventing superintelligence failure modes (misaligned advanced AI systems deployed without adequate alignment/control mechanisms). It aggregates technical expertise, aligns institutional incentives on safety-first development, and establishes governance frameworks intended to slow capability scaling until alignment problems are solved. The coordination problem: absent existential-risk prioritization, competitive pressure in AI development could drive deployment of increasingly capable systems before alignment guarantees exist, creating an irreversible failure scenario.
% TRANSFER_FUNCTION: Transfers institutional authority, research funding, and policy implementation power from present-harm mitigation frameworks (algorithmic fairness, labor protection, surveillance regulation) to existential-risk prevention frameworks (capability evaluation, alignment research, long-horizon governance design). Moves epistemic authority from communities experiencing present harms to x-risk research institutions claiming specialized knowledge of far-future AI capabilities. Extracts governance resources from addressing concrete, measurable suffering in favor of preventing speculative worst-case scenarios.
% ABSENT_VOICES: Workers and communities experiencing present algorithmic harms are structurally excluded: they lack the technical credentials and institutional standing claimed by x-risk institutions and are not seated at governance tables where existential-risk prioritization is debated. Global South communities affected by both present AI harms and future existential-risk consequences have no systematic mechanism for input into governance frameworks designed in wealthy countries. Capability researchers who view existential-risk constraints as overweighting low-probability scenarios relative to near-term benefits are excluded from the frame's legitimacy.
% DISAPPEARANCE_RATIONALE: X-risk advocates argue the constraint would be catastrophically missed: absent existential-risk prioritization, competitive development dynamics would drive unaligned superintelligence deployment, resulting in permanent human obsolescence or extinction—the disappearance would be humanity's disappearance. Present-harm advocates argue that if existential-risk prioritization disappeared, governance resources and policy attention would return to addressing concrete suffering of marginalized populations and restraining deployed systems causing measurable harm. The disagreement is not about whether the constraint exists but about whether its absence is catastrophe or relief.
% FOUNDING_PROBLEM: The founding problem: AI capability development was accelerating faster than safety/alignment research; the gap between what systems could do and what humans could verify/control was widening; if deployment of superintelligent systems happened with unaligned objectives, the failure would be irreversible and catastrophic for humanity's future.
% FOUNDING_PROBLEM_CORROBORATION: X-risk institutions (Alignment Research Center, Machine Intelligence Research Institute, Future of Humanity Institute, Anthropic's safety teams) corroborate the founding problem and attest it remains live and increasingly urgent. AI capability researchers and present-harm advocacy communities dispute the problem's salience: they attest that present AI harms are demonstrably occurring in deployed systems and that closing the capability-safety gap is not the bottleneck preventing deployment harm reduction. Academic computer scientists outside alignment-specific communities dispute the capability timelines and reachability assumptions. No independent corroboration exists from affected populations on whose behalf the future-humanity victim set is claimed—no mechanism exists for them to attest the problem.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__existential_risk_reading, contested).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_governance_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__existential_risk_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68 at interval end) because the constraint transfers authority and resources from present-harm frameworks to x-risk frameworks, but this transfer is partially justified by genuine coordination value (superintelligence is a plausible failure mode). Suppression is substantial (0.61) because present-harm voices must be actively silenced or marginalized for the existential-risk frame to remain governing—the constraint requires excluding competing governance narratives. Theater rises steadily (0.38→0.52) because as governance institutions build elaborate risk-assessment infrastructure and red-team testing, the activity increasingly performs safety leadership without proportionally constraining deployment of systems causing measurable present harm. The measurement grid is shared across all three metrics at six time points (0, 3, 6, 12, 18, 25) with no gaps or misalignment. Extractiveness acceleration 0–6 reflects x-risk narrative gaining institutional dominance; plateau 18–25 reflects that dominance stabilizing. Theater acceleration tracks increasing institutionalization without corresponding harm reduction.
 *
 * PERSPECTIVAL GAP:
 *   From the x-risk institutions' seat, this is genuine rope: they are solving a real coordination problem and the extraction is the necessary cost of governance. From the present-harm-affected-populations' seat, this is snare: resources are being extracted from addressing their concrete suffering through a narrative about catastrophic future risks they have no part in preventing or preparing for. From a policy-maker's seat, the gap is acute: both frames make legitimate claims on governance resources, and the existential-risk frame's temporal reach (civilizational) confers rhetorical authority over immediate concerns. The engine computes per-seat types from the structural data: x-risk seat may compute as rope or tangled_rope depending on whether the coordination function is deemed genuine; present-harm seat should compute toward snare; policy-maker seat may compute as piton if the governance infrastructure becomes performative theater without constraint. This perspectival gap is the constraint's core structural feature, not a failure of the classification.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk institutions: d = 0.2 (full beneficiary: they set the agenda, collect the authority and funding, face minimal exit pressure, enjoy institutional deference). AI labs claiming safety leadership: d = 0.25 (beneficiary: they acquire regulatory advantage by adopting x-risk framing; constrained exit: they cannot easily drop the safety positioning without regulatory and reputational cost). Present-harm-affected populations: d = 0.88 (full target: they bear the cost of governance attention diverted away, have trapped exit, no bargaining power). Future humanity: d = 0.95 (ultimate target: bears existential consequences if assumption wrong, cannot negotiate). Policy-makers: d = 0.5 (symmetric: they must balance competing legitimate claims; their power is institutional so they can move either direction, but the decision costs them political capital regardless). These directionality values come from beneficiary/victim declarations plus power/exit: beneficiaries with arbitrage exit and institutional power → low d; victims with powerless/trapped exit → high d. No overrides are needed—the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (alignment gap widening) was live in 2015–2020 when the constraint emerged. By 2023–2025, the status is contested: x-risk researchers maintain the gap is live and urgent; AI safety progress and empirical uncertainty about capability timelines have created legitimate disagreement. The constraint persists in governance because it benefits the institutions that set the research agenda and advise policymakers, regardless of whether the founding problem has matured. The theater ratio rising from 0.38 to 0.52 (stage 2) indicates mandatrophy symptoms: the infrastructure persists through performative safety review and risk assessment rather than through solving the problem it was built for. A mandatrophy resolution test: if present-harm governance frameworks had equal institutional resources and policy access, would the x-risk constraint still be chosen for prioritization on its merits, or does it persist because it concentrates authority in the hands of institutions that are incentivized to keep existential risk salient? The constraint's structure suggests the latter—it is a tangled rope where the coordination value is contested and the extraction is becoming increasingly visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    superintelligence_timeline_contingency,
    'What is the probability and timeline for artificial superintelligence achieving capability levels that pose existential risk, and how sensitive is this distribution to present AI development choices?',
    'Empirical test: track capability development timelines, scaling laws, emergent abilities in deployed models, and whether present safety measures correspond to reduced deployment risk. Reconvene prediction markets and expert elicitation at 5-year intervals.',
    'If superintelligence timelines extend beyond governance institutions'' planning horizons (>50 years), the existential-risk reading''s urgency claim weakens and extractiveness rating drops sharply. If timelines compress (<10 years), the extractiveness rating may justify stronger suppression. If the timeline is genuinely uncertain, the theater ratio becomes diagnostic of whether governance is proportionally allocating resources.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(superintelligence_timeline_contingency, empirical, 'The credibility of the superintelligence timeline assumption undergirding the existential-risk frame.').

omega_variable(
    alignment_solution_reachability,
    'Is alignment of superintelligent AI systems a solvable problem with known or discoverable approaches, or is it theoretically intractable given fundamental limits of control and oversight?',
    'Research progress in formal alignment theory, success in aligning smaller models, discovery of fundamental limits to interpretability and control. Assess whether research directed at existential-risk problems is converging or diverging.',
    'If alignment is solvable but difficult, the existential-risk governance frame is justified and extractiveness is proportional to resource allocation needed. If alignment is theoretically intractable, the constraint may be redirecting resources toward unsolvable problems while ignoring solvable present harms—raising theater_ratio and lowering claimed-type confidence. If reachability is unknown, the frame''s extractiveness depends on how much uncertainty is suppressed in governance narratives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alignment_solution_reachability, empirical, 'The fundamental tractability of AI alignment and whether present research is making progress.').

omega_variable(
    resource_allocation_zero_sum_assumption,
    'Is the allocation of governance resources between present-harm mitigation and existential-risk prevention structurally zero-sum, or can frameworks address both without trade-off?',
    'Examine actual governance budgets and policy implementation: does prioritizing x-risk reduce funding for algorithmic-fairness regulation, labor protection, or surveillance oversight? Can governance infrastructure serve both purposes simultaneously?',
    'If the allocation is zero-sum (governance attention is finite, choosing x-risk means deprioritizing present harms), the victim set and extraction calculation are accurate as authored. If the allocation is not zero-sum (both can be advanced simultaneously), the victim set shrinks and the constraint''s extractiveness drops—present-harm-affected populations are no longer paying the cost of the transfer. If governance is genuinely ignorant of the zero-sum status, theater increases because the frame obscures whether tradeoffs are being made.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_zero_sum_assumption, empirical, 'Whether existential-risk prioritization structurally requires deprioritizing present-harm governance.').

omega_variable(
    epistemic_access_and_authority_asymmetry,
    'Do x-risk institutions possess genuinely privileged epistemic access to far-future AI capabilities that justifies their governance authority, or is their authority social/institutional rather than epistemically grounded?',
    'Track which x-risk predictions have been falsified or postponed, which present-harm predictions from marginalized researchers have been vindicated. Examine whether x-risk institutions'' models have superior predictive performance or whether their authority derives from institutional positioning and funder alignment.',
    'If epistemic access is genuine and predictions accurate, the authority asymmetry is justified and governance prioritization is rational. If epistemic access is claimed but unvalidated, the authority is institutional theater and the constraint''s suppression of competing voices is harder to justify. This determines whether the constraint''s extractiveness is the cost of coordination or the cost of manufactured consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_access_and_authority_asymmetry, conceptual, 'Whether the governance authority of x-risk institutions is epistemically or institutionally grounded.').

omega_variable(
    beneficiary_capture_and_solutionspace_bias,
    'Are the research agendas and governance frameworks championed by x-risk beneficiary institutions oriented toward solutions that preserve their institutional role and funding, rather than toward solutions that would minimize existential risk if they existed outside those institutions?',
    'Examine which governance proposals are advanced and which are suppressed: are proposed solutions always those requiring institutional mediation (safety research, model evaluation, governance frameworks led by alignment institutions)? Are solutions that would minimize existential risk through decentralization, transparency, or capability constraint equally championed?',
    'If beneficiary capture is operating, the constraint is extracting from governance in the guise of preventing catastrophe. The extractiveness rating rises toward snare territory. If the constraint''s solutions are neutral about institutional form, the extraction is a genuine coordination cost. This omega addresses whether the mandatrophy analysis (governance infrastructure persisting through theater) is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_and_solutionspace_bias, preference, 'Whether existential-risk governance frameworks are biased toward solutions that preserve the institutional role of x-risk researchers.').

omega_variable(
    kernel_reading_committer_contest,
    'Which reading of the ai_risk_governance_priority kernel is structurally true: the existential-risk prioritization (this reading), the present-harm prioritization (near_term_harms_reading), or the integration attempt (bridge_reading)?',
    'This is a committer-frame omega, not empirically resolvable. The resolution lies in how governance institutions and affected communities frame the legitimacy of competing priorities. It is resolved by policy implementation—whichever reading becomes crystallized in law and practice becomes the institutionally-enacted kernel reading.',
    'If existential-risk reading persists as governing kernel, x-risk institutions capture authority and resources; present-harm governance is subordinated. If near_term_harms reading becomes governing, existential-risk research loses institutional deference. If bridge_reading succeeds, both frameworks become coequal in governance, reducing the extractiveness of this constraint. The outcome depends on political dynamics among stakeholders with unequal power.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_contest, preference, 'The committer-contest structure of the kernel reading—which framing of AI governance risk becomes institutionalized as legitimate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ai_r_tr_t3, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 3, 0.41).
narrative_ontology:measurement(ai_r_tr_t6, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 6, 0.45).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 12, 0.5).
narrative_ontology:measurement(ai_r_tr_t18, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 18, 0.51).
narrative_ontology:measurement(ai_r_tr_t25, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 25, 0.52).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ai_r_be_t3, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(ai_r_be_t6, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(ai_r_be_t18, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 18, 0.66).
narrative_ontology:measurement(ai_r_be_t25, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(ai_r_su_t3, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(ai_r_su_t6, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 6, 0.56).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 12, 0.59).
narrative_ontology:measurement(ai_r_su_t18, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 18, 0.6).
narrative_ontology:measurement(ai_r_su_t25, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 25, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__existential_risk_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__bridge_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_model_deployment_governance).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, algorithmic_fairness_regulation).

% DUAL FORMULATION NOTE:
% The ai_risk_governance_priority kernel decomposes into three structurally distinct constraints, each with its own ε, beneficiary/victim structure, and institutional implications. The existential-risk reading (this constraint) prioritizes long-tail worst-case scenarios; the near-term-harms reading prioritizes demonstrated algorithmic damage to marginalized populations now; the bridge reading attempts to treat them as non-zero-sum. They are not different measurements of the same constraint—they are three different constraints whose legitimacy and resource allocation compete. Each reading instantiates a different constraint with different referents: existential-risk reading measures ε for the standing arrangement that prioritizes x-risk governance; near-term-harms reading measures ε for the standing arrangement that subordinates existential-risk governance to present-harm mitigation; bridge reading measures ε for an attempted integration framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
