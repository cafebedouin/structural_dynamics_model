% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__existential_risk_reading, []).

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
 *   constraint_id: ai_safety_commitment__existential_risk_reading
 *   human_readable: Existential Risk Prevention via Superintelligence Alignment
 *   domain: technology/governance/risk assessment
 *
 * SUMMARY:
 *   The existential risk reading of AI safety frames the problem as
 *   preventing extinction-level outcomes from superintelligent misaligned
 *   systems. This reading instantiates one of three contested interpretations
 *   of what 'AI safety' means (alongside near_term_harms_reading and
 *   dual_priority_reading). The reading is ONE CONSTRAINT generated from the
 *   ai_safety_commitment kernel—a stabilized commitment (the UN, OpenAI,
 *   major governance bodies invoke 'AI safety') that different parties
 *   interpret through different readings. This story models ONLY the
 *   existential risk reading: its ε-invariant referent is the standing
 *   arrangement under existential-risk contest—the resource allocation and
 *   governance priorities that follow if existential risk is the paramount
 *   concern. The sibling readings are NOT described here; they are other
 *   constraint stories in the constraint family.
 *
 * KEY AGENTS:
 *   - Existential risk researchers: define research agenda; institutional power; control narrative legitimacy
 *   - Capability researchers: benefit from deferral of present-day accountability; maintain development velocity
 *   - Future humans (potentially infinite): stated beneficiaries; zero voice in present governance; speculative victim status
 *   - Victims of documented present harms: invisible in existential risk framing; resource-starved near-term researchers; extraction through defunding
 *   - Global South governments: excluded from superintelligence-framed governance; experience extractive AI application; constrained exit
 *   - AI safety funding gatekeepers: control capital flows; concentrate resources on existential reading; institutional arbitrage
 *   - Regulatory authorities: receive testimony biased toward existential framing; inattention to present harms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__existential_risk_reading, 0.78).
domain_priors:suppression_score(ai_safety_commitment__existential_risk_reading, 0.62).
domain_priors:theater_ratio(ai_safety_commitment__existential_risk_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__existential_risk_reading, "Existential Risk Prevention via Superintelligence Alignment").
narrative_ontology:topic_domain(ai_safety_commitment__existential_risk_reading, "technology/governance/risk assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__existential_risk_reading, '56c94335-45fe-4597-b2d0-f496cc2ad202').
narrative_ontology:cs_kernel_codification('56c94335-45fe-4597-b2d0-f496cc2ad202', distributed).
narrative_ontology:cs_authority_grounding('56c94335-45fe-4597-b2d0-f496cc2ad202', extraction).
narrative_ontology:cs_interpretation_layer_present('56c94335-45fe-4597-b2d0-f496cc2ad202').
narrative_ontology:cs_reading_relation('56c94335-45fe-4597-b2d0-f496cc2ad202', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('56c94335-45fe-4597-b2d0-f496cc2ad202', ai_safety_commitment__dual_priority_reading, influences).
narrative_ontology:cs_axiom('56c94335-45fe-4597-b2d0-f496cc2ad202', foundational, superintelligence_existential_threat).
narrative_ontology:cs_axiom_status(superintelligence_existential_threat, holdable).
narrative_ontology:cs_axiom_grounding('56c94335-45fe-4597-b2d0-f496cc2ad202', superintelligence_existential_threat, empirically_contingent).
narrative_ontology:cs_axiom('56c94335-45fe-4597-b2d0-f496cc2ad202', foundational, longtermist_moral_priority).
narrative_ontology:cs_axiom_status(longtermist_moral_priority, holdable).
narrative_ontology:cs_axiom_grounding('56c94335-45fe-4597-b2d0-f496cc2ad202', longtermist_moral_priority, deontological).
narrative_ontology:cs_reference_frame('56c94335-45fe-4597-b2d0-f496cc2ad202', pre_superintelligence_alignment_solved).
narrative_ontology:cs_drift_state('56c94335-45fe-4597-b2d0-f496cc2ad202', contemporary_capability_acceleration, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('56c94335-45fe-4597-b2d0-f496cc2ad202', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__existential_risk_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, future_humanity_conditional_alignment_success).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, present_algorithmic_accountability_research).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, near_term_harm_remediation_resourcing).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, global_south_ai_governance_participation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, capability_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, capability_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define research agendas around superintelligence alignment: interpretability, RLHF safety, formal verification, AI governance mechanisms for pause or slowdown. Control peer-review decisions at ICML/NeurIPS safety tracks and specialized conferences (ICML-Safe, NeurIPS Red Teaming Track, FHI, CHAI, Berkeley CHAI). Set funding priorities by controlling narrative authority over what 'AI safety' means. Their institutional position derives from existential risk framing's legitimacy; alternative framings (near-term harms as primary) would redistribute their research authority and funding allocation.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, existential_risk_focused_researchers, agenda_setter,
    institutional, civilizational, mobile, global).

% Benefit from existential risk framing by deferring present-day accountability for model deployment: the 'alignment is paramount and unsolved' narrative justifies releasing capability benchmarks without harm assessments, deploying frontier models with minimal safety constraints, and framing harm mitigation as secondary to capability advancement. They also pay in reputation (must perform safety-washing, engage with existential researchers) but gain primary strategic objective of continuous scaling. The existential reading vindicates their trajectory by positioning capability work as foundational to long-term safety.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, capability_researchers, beneficiary,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__existential_risk_reading, capability_researchers, payer).

% The stated beneficiary of the constraint: their existence and flourishing depend on superintelligent systems being aligned before achieving capabilities that enable extinction-level harm. They are infinitely numerous (all future potential humans) but infinitely voiceless: they cannot participate in research agenda-setting, cannot testify about their interests, cannot verify whether current technical interventions (RLHF, interpretability research) actually reduce their risk. Their benefit is highly speculative, conditional on: (a) superintelligence being developed, (b) misalignment being technically possible, (c) current research actually solving alignment, (d) solutions being deployed before catastrophe. Multiple failure points mean their stated beneficiary status may be illusory.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, future_humanity_conditional_alignment_success, beneficiary,
    powerless, civilizational, trapped, universal).

% Research communities studying documented algorithmic harms (FAccT, AIES, AI Now Institute, Data & Society) work on fairness, transparency, labor impacts, and accountability mechanisms for deployed systems. The existential risk reading subordinates their work as 'necessary but secondary': present harms are framed as inevitable costs of progress toward superintelligent alignment rather than coordination failures warranting immediate remedy. They compete for funding and venue attention against existential-risk work and lose on civilizational-urgency grounds. Their exit options are constrained: pivoting to existential-risk framing is costly (requires retraining in technical safety), staying in near-term harm framing is professionally de-prioritized.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, present_algorithmic_accountability_research, payer,
    moderate, biographical, constrained, global).

% Victims of documented algorithmic harms across jurisdictions: those experiencing discriminatory hiring decisions (resume-screening bias), lending denial (credit-risk models), misinformation amplification targeting specific communities, predictive surveillance on reproductive autonomy and immigration status, labor displacement from automation without retraining. The existential risk reading extracts from this population through resource starvation: AI safety funding diverts from harm remediation toward speculative alignment research. They are trapped because they cannot exit algorithmic systems affecting their lives regardless of their participation in governance.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, near_term_harm_remediation_resourcing, payer,
    powerless, immediate, trapped, global).

% Governments and civil society in global South countries (India, Nigeria, Kenya, Indonesia, Brazil, Mexico) experience extractive application of AI systems (labor optimization pushing informal workers offline, extraction algorithm for resource-management in colonial-pattern economics, surveillance for social control). They are excluded from existential-risk research conversations and AI governance forums framed around superintelligence discontinuity—issues treated as high-technical-IQ questions rather than political economy problems. The constraint's framing justifies their continued exclusion: superintelligence is a global-North technical problem, 'below' immediate governance concern. Their constrained exit: they cannot opt out of AI application to their economies; they can advocate for inclusion but face institutional barriers (language, venue access, epistemic authority gaps).
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, global_south_ai_governance_participation, payer,
    organized, generational, constrained, global).

% Control capital flows to AI safety work: Open Philanthropy, Effective Altruism community funds, AI governance centers (Center for AI Safety, Institute for AI Policy and Strategy), national security establishments (RAND, Atlantic Council), university safety labs (Berkeley CHAI, MIT SERC, Stanford HAI). They allocate capital overwhelmingly to existential-risk-reading work (interpretability labs, governance pause research, formal alignment research) and constrain funding for near-term harm remediation and accountability mechanisms. The existential risk reading's legitimacy enables this allocation: civilizational stakes justify concentration. Their arbitrage exit: can reallocate funds to different frames if priorities shift, but currently benefit from existential framing dominance.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, ai_safety_funding_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, global).

% A propositional entity (not a human actor), representing the moral framework underlying the existential reading: expected-value calculus over infinite future populations, impersonal harms, moral status of conditional-on-existence beings. The constraint's legitimacy depends on longtermism's axioms being correct—particularly that present resource allocation should be optimized for preventing extinction rather than addressing documented present harms. This is included for narrative completeness per OQ-64 but excluded from beneficiary derivation (it collects no rents).
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, longtermist_moral_philosophy_tradition, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ai_safety_commitment__existential_risk_reading, longtermist_moral_philosophy_tradition).

% Receive testimony and technical briefs on AI risk from existential-risk researchers (Stanford, Berkeley, Open Philanthropy networks), near-term harm researchers (AI accountability organizations), and capability researchers (OpenAI, DeepMind, Anthropic). Face pressure to regulate on both existential and present-harm grounds but lack internal expertise to independently evaluate competing claims. The existential risk reading's framing (superintelligence as the primary target, technical control problems, long timescales) biases regulatory attention toward speculative long-term governance frameworks while immediate harms (hiring discrimination, labor displacement) accumulate under-regulated.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, regulatory_authorities, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__existential_risk_reading, existential_risk_focused_researchers).
narrative_ontology:fixing_cost_class(ai_safety_commitment__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the research and governance apparatus to focus computational resources on the technical challenge of aligning superintelligent systems before they are deployed at scale—a genuine collective-action problem: individual capability researchers lack incentive to decelerate work on unresolved alignment problems, so centralized governance must mandate pause or slowdown conditional on alignment progress.
% TRANSFER_FUNCTION: Moves research funding, policy attention, and governance authority away from documented present-day algorithmic harms (hiring discrimination, labor displacement, misinformation) and algorithmic accountability mechanisms (impact assessments, auditing) toward speculative superintelligence interventions (RLHF safety, interpretability proofs, AI governance for managed slowdown). The transfer is from present-harm researchers, vulnerable populations experiencing documented injuries, and global-south participation in AI governance toward existential-risk-focused institutions and capability researchers whose work is vindicated by existential framing.
% ABSENT_VOICES: Victims of documented present algorithmic harms have no seat at existential risk research forums; they cannot testify that their injuries are not worth the resource trade-off. Global-South governments and civil society cannot participate in governance conversations that assume superintelligence as the primary problem. Near-term harm researchers are included in some conversations but their budget is subordinated. Labor advocates, disability justice communities, and anti-surveillance movements experience the constraint's effects but are not named in safety frameworks.
% DISAPPEARANCE_RATIONALE: If the existential risk reading and its resource-allocation consequences disappeared overnight, present-day AI accountability and near-term harm remediation would move into the center of gravity: funding would flow to algorithmic auditing, fairness interventions, labor protection, surveillance resistance, and global-south governance participation. The research apparatus would reorganize around documented harms rather than speculative extinction scenarios. Existential risk researchers attest the world would be less prepared for superintelligence discontinuity; longtermists attest extinction risk would rise; near-term harm advocates attest present injuries would finally be addressed at scale. The constraint's disappearance is contested because parties dispute which outcome matters.
% FOUNDING_PROBLEM: In the early 2010s, AI capability progress was accelerating without corresponding safety research: language models, computer vision, and reinforcement learning systems were being scaled without technical solutions to alignment, interpretability, or containment. A small number of researchers (Bostrom, Yudkowsky, later Tegmark, Christiano) argued that the risk of superintelligent misaligned systems posed existential stakes greater than any present-day problem, and that research focus needed to shift toward long-horizon technical safety work.
% FOUNDING_PROBLEM_CORROBORATION: Existential risk researchers attest that capability progress continues to outpace safety progress and that the original founding problem is live and intensifying. Capability researchers increasingly acknowledge alignment as technically important but dispute its civilizational priority relative to capability advancement. Near-term harm researchers and affected communities attest that the founding problem was articulated by a narrow technical elite without input from those experiencing present AI harms, and that the 'alignment is paramount' narrative has become a cover story for deferring accountability. External corroboration: reports from AI accountability organizations (AI Now, Center for AI Safety), academic work in fairness and justice-oriented AI studies, labor union testimony, and civil rights organizations document that the existential risk reading's dominance has redirected governance away from addressing documented harms.
narrative_ontology:disappearance_verdict(ai_safety_commitment__existential_risk_reading, contested).
narrative_ontology:founding_problem_status(ai_safety_commitment__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_safety_commitment__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__existential_risk_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78, rising from 0.35 over 14 years) measures the extent to which resource allocation and governance are decoupled from the stated coordination function (alignment of superintelligent systems) and serve instead to concentrate research authority and defer accountability. The rise from 2012 to 2026 reflects three dynamics: (1) capability progress outpaced safety progress despite increased safety research, suggesting extraction is accumulating (the coordination function fails while the arrangement persists); (2) funding concentration in existential risk work grew despite expanding documented harms in deployed systems (asymmetric resource reallocation); (3) suppression requirement rose (active enforcement of the existential-risk-paramount framing against competing frames required more institutional effort as contradictions became visible). Theater ratio (0.68, rising from 0.22) reflects the constraint's shift from genuine research coordination (early 2010s: small community trying to solve real technical problem) toward performative alignment (2025–2026: visible safety-washing by capability companies, regulatory theater, 'alignment research' that doesn't slow deployment). The gap between rising theater and stable (or rising) extractiveness indicates the constraint is degrading functionally while persisting institutionally—Piton-adjacent dynamics. Suppression (0.62) captures active enforcement: near-term harm researchers are excluded from high-status venues; capability researchers face pressure to downplay accountability failures; funding decisions enforce the existential-risk-paramount narrative against near-term frames. Accessibility collapse (0.72) reflects the difficulty of switching research focus once committed to existential risk work (identity-locked researchers, sunk career investments, institutional prestige structures aligned with existential framing). Resistance (0.58) measures the real push-back from near-term harm communities, labor advocates, global-South governments, and some academic ethicists—substantial but institutionally weaker than existential-risk-focused institutional power.
 *
 * PERSPECTIVAL GAP:
 *   The existential risk researcher seat and the near-term harm researcher seat compute radically different types from the same constraint. The existential-risk seat: genuine coordination function (align superintelligence before deployment), real collective-action problem (no individual researcher has incentive to decelerate), some asymmetric extraction justified by civilizational stakes—computationally Tangled Rope from their seat, with the extraction defended as necessary cost. The near-term harm researcher seat: no perceivable coordination benefit (present harms are already decoupled from safety research), asymmetric extraction (their work is defunded in favor of speculative work), active suppression (their research venues are de-prioritized), identity-locked exit (reframing outside existential paradigm costs professional legitimacy)—computationally Snare from their seat. The future humans seat: speculative beneficiary with zero voice and trapped exit (they cannot exit the civilization-level decision)—computationally an asymmetric power structure sustained by their absence from the conversation. The engine computes these per-seat divergences from the structural data; the claim (Tangled Rope, existential reading) represents the agenda-setter's frame, not an objective classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Existential risk researchers: directionality low (near-0), near-beneficiary end. They control the research agenda, allocate authority, publish in high-status venues, and their institutional position is strengthened by existential framing. They have mobile exit options (can shift focus to capability work without losing status). Power: institutional. Capability researchers: directionality moderate-low (0.2–0.35). They benefit from deferred accountability but face rhetorical pressure to engage with alignment concerns. Exit: mobile (can pursue capability work with less safety focus elsewhere). Power: institutional. Near-term harm researchers: directionality high (0.72–0.84). They lose funding and status, face suppression in venue selection, have constrained exit (reframing outside existential paradigm is career-costly). Power: moderate. Victims of documented harms: directionality highest (0.88+). They bear costs through resource starvation on remediation, have constrained exit (they cannot exit algorithmic systems affecting their lives), zero participation in governance. Power: powerless. Global South governments: directionality high (0.75+). Excluded from governance, experience extractive AI application, have constrained exit (AI systems are applied to them regardless of consent). Power: organized. Future humans: directionality is maximal but paradoxical (1.0 by definition—they are the stated beneficiary, yet they have zero voice, trapped exit, and the benefit is speculative and conditional on alignment success). Power: powerless. The directionality structure reveals the existential reading's actual operation: it benefits institutional researchers and capability companies while imposing extraction on powerless present populations (documented harm victims) and future populations (speculative superintelligence risk). No overrides needed; the derivation is clean.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (2012–2015) was live: capability progress was outpacing safety research, and there was no coherent research agenda addressing superintelligence alignment. By 2022–2026, the founding problem status is contested. Existential-risk researchers attest it is LIVE and intensifying (GPT-4 capabilities surprised many, scaling shows no sign of slowing, alignment remains technically unsolved). Near-term harm researchers and affected communities attest it is DEAD or displaced: the constraint's founding purpose was to redirect research toward superintelligence, but superintelligence remains speculative while documented AI harms have proliferated (discriminatory hiring, labor displacement, misinformation, surveillance). The world_rearranges verdict (the constraint matters) contradicts the DEAD status (the founding problem is resolved)—a classic zombie-constraint signature, Mandatrophy candidate. The persistence mechanism is institutional capture: existential-risk researchers and funding gatekeepers have consolidated authority over 'AI safety' meaning. The constraint persists because they benefit from it, not because the founding problem remains. This is MANDATROPHY RESOLVED = true candidate, but the schema flags it as a mismatch signal (founding_problem_status=contested + disappearance_verdict=world_rearranges triggers the cross-frame falsification the engine uses for mandatrophy detection, not an author-declared boolean). The theater ratio rising (0.22 → 0.68) while extractiveness rises (0.35 → 0.78) is Piton-adjacent: the constraint is increasingly performative (safety-washing, regulatory theater) while simultaneously extracting more resources from alternative framings. This is not stable Piton behavior—it is a constraint in degradation, where the original coordination function (alignment research) is decoupling from the resource flow (capability companies claim alignment while scaling unresolved systems, funding goes to labs that don't slow deployment).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    superintelligence_discontinuity_empirical_status,
    'Is superintelligent misaligned systems a plausible extinction-level risk, or a low-probability tail scenario that should be weighted differently in resource allocation?',
    'Convergence in AI capabilities research on scaling laws, emergent capabilities, and loss-of-control scenarios. Formal specification of ''superintelligence'' and extinction mechanisms (value lock-in, instrumental convergence failure, corrigibility loss). Empirical evidence from language model behavior and alignment research progress.',
    'If extinction risk is empirically low-probability, the existential-risk reading''s justification for resource concentration collapses; resource allocation should rebalance toward documented present harms. If extinction risk is high-probability and near-term, the reading''s framing is vindicated and present-harm research remains subordinate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(superintelligence_discontinuity_empirical_status, empirical, 'Whether superintelligence poses extinction-level risk plausible enough to justify resource-allocation centralization.').

omega_variable(
    moral_status_of_potential_future_beings,
    'Are infinite future human beings (conditional on survival) equal moral subjects to present-day humans bearing documented harms from current AI systems?',
    'Philosophical convergence on person-affecting vs. impersonal harms, moral status of potential beings, expected-value calculus over infinite populations. Jurisprudential precedent in environmental and future-harm law.',
    'If future potential beings do not have equal moral status, the existential-risk reading''s beneficiary claim is weakened and resource allocation toward present remediation is justified. If they do, the reading is vindicated and speculative future prevention justifies present-harm trade-offs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_status_of_potential_future_beings, preference, 'Competing moral frameworks for weighting present vs. future harms.').

omega_variable(
    alignment_technical_solvability_under_timeline_pressure,
    'Can superintelligent alignment be solved technically (RLHF, interpretability, formal verification) before superintelligent systems are deployed at scale, given observed capability progress trajectories?',
    'Progress in interpretability (mechanistic understanding of language models, circuit analysis); success or failure of alignment approaches (RLHF scaling limits, reward hacking, deceptive alignment evidence); observed deployment timelines for frontier models.',
    'If alignment is not technically solvable on deployment timelines, the existential-risk reading''s coordination function is illusory—the constraint is pure extraction dressed as safety coordination. If alignment is solvable, the reading''s claim is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alignment_technical_solvability_under_timeline_pressure, empirical, 'Whether existential risk can be addressed through technical research or requires pause/slowdown governance.').

omega_variable(
    extraction_mechanism_vs_coordination_mechanism,
    'Does the existential-risk framing''s resource concentration toward interpretability and governance-for-pause actually reduce extinction risk, or does it primarily suppress near-term harm research while allowing capability advancement to continue?',
    'Empirical comparison: jurisdictions/institutions that adopted existential-risk framing vs. near-term-harm framing. Measurement of: (a) alignment research progress (tractable interpretability breakthroughs, formal alignment proofs, deployment pause success); (b) near-term harm reduction (measured bias in deployed systems, labor displacement rates, misinformation spread); (c) capability scaling trajectories. Timeline: 2025–2030.',
    'If existential-risk framing correlates with alignment progress and deployment slowdown, it is genuine coordination. If it correlates with capability acceleration and harm accumulation, it is extraction masquerading as coordination—a Snare or Tangled Rope collapsing toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_vs_coordination_mechanism, empirical, 'Whether the constraint''s coordination function is operating or has decoupled from its resource flow.').

omega_variable(
    competing_kernel_readings_foreclosure,
    'Can a single party or framework simultaneously hold the existential-risk reading and the near-term-harms reading as equally-weighted priorities (dual priority kernel reading), or do these readings foreclose each other?',
    'Organizational behavior: how many institutions officially adopt dual-priority framing? How do their resource allocation, research venue selection, and governance positions differ from pure existential-risk institutions? Philosophical test: are the core axioms of existential reading (longtermism, superintelligence discontinuity, instrumental convergence) logically compatible with near-term reading''s axioms (present algorithmic accountability, immediate harm reduction, procedural justice)?',
    'If readings foreclose each other, they are incompatible strategic commitments and resource allocation is zero-sum. If they coexist, the institutional dominance of existential framing becomes a choice rather than a logical necessity—the extraction becomes policy-contingent rather than structurally forced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_kernel_readings_foreclosure, conceptual, 'Whether competing kernel readings are structurally incompatible or strategically chosen.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.62) of near-term harm framing structural (funding barriers, venue exclusion, institutional barriers) or internalized (near-term researchers have adopted existential framing as legitimate, now suppress their own alternatives)?',
    'Post-suppression trajectory: if existential-risk funding and venue dominance were removed (regulatory mandate, funding reallocation), would near-term harm research immediately expand or would researchers continue suppressing themselves? Interview data and behavioral evidence of adoption vs. coercion.',
    'If suppression is structural, removing institutional barriers would restore near-term research. If suppression is internalized, the constraint persists through belief adoption rather than coercion—identity-locked researchers would need deprogramming, not just resource reallocation. This determines whether the constraint is escapable or self-reproducing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized, affecting escape trajectory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__existential_risk_reading, 2012, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t2012, ai_safety_commitment__existential_risk_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement(ai_s_tr_t2016, ai_safety_commitment__existential_risk_reading, theater_ratio, 2016, 0.38).
narrative_ontology:measurement(ai_s_tr_t2019, ai_safety_commitment__existential_risk_reading, theater_ratio, 2019, 0.51).
narrative_ontology:measurement(ai_s_tr_t2022, ai_safety_commitment__existential_risk_reading, theater_ratio, 2022, 0.63).
narrative_ontology:measurement(ai_s_tr_t2025, ai_safety_commitment__existential_risk_reading, theater_ratio, 2025, 0.66).
narrative_ontology:measurement(ai_s_tr_t2026, ai_safety_commitment__existential_risk_reading, theater_ratio, 2026, 0.68).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t2012, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2012, 0.35).
narrative_ontology:measurement(ai_s_be_t2016, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2016, 0.52).
narrative_ontology:measurement(ai_s_be_t2019, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2019, 0.64).
narrative_ontology:measurement(ai_s_be_t2022, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2022, 0.72).
narrative_ontology:measurement(ai_s_be_t2025, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2025, 0.76).
narrative_ontology:measurement(ai_s_be_t2026, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2026, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t2012, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2012, 0.28).
narrative_ontology:measurement(ai_s_su_t2016, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2016, 0.41).
narrative_ontology:measurement(ai_s_su_t2019, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2019, 0.48).
narrative_ontology:measurement(ai_s_su_t2022, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2022, 0.57).
narrative_ontology:measurement(ai_s_su_t2025, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2025, 0.61).
narrative_ontology:measurement(ai_s_su_t2026, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__existential_risk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__existential_risk_reading, 0.18).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, ai_safety_commitment__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, ai_safety_commitment__dual_priority_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, ai_capability_scaling_governance).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, algorithmic_accountability_framework).

% DUAL FORMULATION NOTE:
% The ai_safety_commitment kernel decomposes into three constraint stories: existential_risk_reading (this story), near_term_harms_reading (present-day algorithmic harms as primary safety concern), and dual_priority_reading (both framings as non-competing). Each story authors ε from its own reading's lights: existential reading sees high ε on present-harm research (extraction through defunding) and low ε on interpretability/pause-governance (justified coordination); near-term reading sees high ε on existential framing (resource diversion) and low ε on algorithmic auditing (genuine accountability). The three are linked because institutional dominance of existential reading structurally constrains near-term research capacity and shapes what 'AI safety' means across the field. ε is reading-indexed: the existential reading's ε=0.78 is for the standing arrangement under existential-risk contest, not for the near-term-harm reading's endorsed alternative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_safety_commitment__existential_risk_reading, institutional, 0.84).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
