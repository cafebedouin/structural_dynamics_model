% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: AI Safety: Existential Risk Prevention
 *   domain: AI Safety/Technology Governance/Risk Assessment
 *
 * SUMMARY:
 *   This constraint story instantiates the 'existential risk' reading of the
 *   broader 'AI safety commitment' kernel. It focuses on the imperative to
 *   prevent extinction-level outcomes from misaligned superintelligent AI,
 *   often advocating for significant technical interventions (e.g., RLHF,
 *   interpretability) and strong AI governance (e.g., pauses, slowdowns).
 *   This reading prioritizes a speculative, high-impact future risk over more
 *   immediate, documented harms from deployed AI systems. The high
 *   extractiveness and suppression reflect the significant costs imposed on
 *   current AI development and potential future freedoms in service of this
 *   long-term goal.
 *
 * KEY AGENTS:
 *   - Existential Risk Researchers: Primary agenda-setters and beneficiaries (organized/identity_locked) — define the problem and direct resources.
 *   - Unaligned AI Developers: Primary payers (powerful/constrained) — bear costs of potential regulation and research redirection.
 *   - Humanity (conditional on alignment): Abstract beneficiary (powerless/trapped) — benefits from hypothetical risk avoidance.
 *   - Near-Term Harms Advocates: Excluded (organized/constrained) — their concerns are often deprioritized.
 *   - Future Generations (under restrictive governance): Payer/Victim (powerless/trapped) — bear costs of foregone innovation or constrained futures.
 *   - AI Governance Bodies: Agenda-setters (institutional/constrained) — implement policies influenced by this framing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__existential_risk_reading, 0.78).
domain_priors:suppression_score(ai_safety_commitment__existential_risk_reading, 0.7).
domain_priors:theater_ratio(ai_safety_commitment__existential_risk_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__existential_risk_reading, "AI Safety: Existential Risk Prevention").
narrative_ontology:topic_domain(ai_safety_commitment__existential_risk_reading, "AI Safety/Technology Governance/Risk Assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__existential_risk_reading, '2584b33e-38ed-4302-b561-c87ece80560e').
narrative_ontology:cs_kernel_codification('2584b33e-38ed-4302-b561-c87ece80560e', distributed).
narrative_ontology:cs_authority_grounding('2584b33e-38ed-4302-b561-c87ece80560e', expertise).
narrative_ontology:cs_interpretation_layer_present('2584b33e-38ed-4302-b561-c87ece80560e').
narrative_ontology:cs_reading_relation('2584b33e-38ed-4302-b561-c87ece80560e', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('2584b33e-38ed-4302-b561-c87ece80560e', ai_safety_commitment__dual_priority_reading, coexists_with).
narrative_ontology:cs_axiom('2584b33e-38ed-4302-b561-c87ece80560e', foundational, existential_risk_is_paramount).
narrative_ontology:cs_axiom_status(existential_risk_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('2584b33e-38ed-4302-b561-c87ece80560e', existential_risk_is_paramount, deontological).
narrative_ontology:cs_axiom('2584b33e-38ed-4302-b561-c87ece80560e', secondary, misaligned_superintelligence_is_imminent_threat).
narrative_ontology:cs_axiom_status(misaligned_superintelligence_is_imminent_threat, holdable).
narrative_ontology:cs_axiom_grounding('2584b33e-38ed-4302-b561-c87ece80560e', misaligned_superintelligence_is_imminent_threat, empirically_contingent).
narrative_ontology:cs_reference_frame('2584b33e-38ed-4302-b561-c87ece80560e', humanity_survives_aligned_ai).
narrative_ontology:cs_drift_state('2584b33e-38ed-4302-b561-c87ece80560e', contemporary_ai_acceleration, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2584b33e-38ed-4302-b561-c87ece80560e', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(ai_safety_commitment__existential_risk_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, humanity_conditional_on_alignment).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, existential_risk_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, unaligned_ai_developers).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, future_generations_under_restrictive_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively define, research, and advocate for the prevention of extinction-level outcomes from misaligned superintelligent AI. Their careers and funding are often tied to the urgency and perceived severity of this risk. They seek to influence policy and direct resources towards technical alignment and governance solutions.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, existential_risk_researchers, agenda_setter,
    organized, generational, identity_locked, global).

% These are developers and organizations focused on rapid AI advancement, often without explicit prioritization of existential risk mitigation. They bear the costs of potential regulatory slowdowns, research redirection, and public scrutiny driven by the existential risk narrative. Their exit options are limited by the global nature of AI development and increasing calls for regulation.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, unaligned_ai_developers, payer,
    powerful, biographical, constrained, global).

% Represents the collective future of humanity, which is posited to benefit from successful AI alignment and the prevention of existential catastrophe. This beneficiary is conditional and abstract, as its 'benefit' is the avoidance of a hypothetical future harm.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, humanity_conditional_on_alignment, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_safety_commitment__existential_risk_reading, humanity_conditional_on_alignment).

% These are researchers and activists who prioritize addressing present-day harms from deployed AI systems (e.g., bias, discrimination, labor displacement). They are often excluded from or marginalized in discussions dominated by existential risk, feeling their concerns are deprioritized or seen as distractions.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, near_term_harms_advocates, excluded,
    organized, biographical, constrained, global).

% These are hypothetical future populations who might bear the costs of overly restrictive AI governance, such as foregone technological benefits, reduced innovation, or a permanently constrained future due to extreme caution in AI development. Their 'payment' is in missed opportunities or reduced autonomy.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, future_generations_under_restrictive_governance, payer,
    powerless, generational, trapped, universal).

% These are national and international organizations tasked with regulating AI. They are influenced by the existential risk narrative, leading them to consider policies like licensing, compute caps, or research pauses, which can impose significant costs on developers and potentially future societies.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, ai_governance_bodies, agenda_setter,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__existential_risk_reading, existential_risk_researchers).
narrative_ontology:fixing_cost_class(ai_safety_commitment__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global efforts and resources towards identifying and mitigating hypothetical extinction-level risks from advanced AI, ensuring humanity's long-term survival by aligning AI with human values.
% TRANSFER_FUNCTION: Transfers significant intellectual, financial, and regulatory resources from immediate AI applications and other safety concerns towards speculative, long-term technical alignment research and restrictive governance proposals. It also potentially transfers autonomy and innovation opportunities from future generations to current risk-averse governance structures.
% ABSENT_VOICES: Advocates for near-term AI harms (bias, discrimination, labor displacement) are often sidelined. Those who believe in rapid, open AI development for societal benefit, or who dispute the imminence/severity of existential risk, are also often excluded from the core conversation. Future generations, the ultimate beneficiaries/victims, are inherently absent.
% DISAPPEARANCE_RATIONALE: If the commitment to preventing existential AI risk vanished overnight, AI development would likely accelerate without significant alignment safeguards or regulatory caution. This could lead to the very catastrophic outcomes it seeks to prevent, or to a world where near-term harms are unmitigated due to lack of focus, fundamentally reorganizing humanity's future trajectory.
% FOUNDING_PROBLEM: The theoretical possibility of future superintelligent AI systems becoming misaligned with human values, leading to an uncontrollable intelligence explosion that could cause human extinction or irreversible disempowerment.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by a significant portion of the AI research community (e.g., MIRI, FHI, Alignment Research Center, OpenAI's Superalignment team) and prominent public figures (e.g., Elon Musk, Sam Altman). However, it is contested by other AI researchers and ethicists who view it as speculative, overblown, or a distraction from more immediate, verifiable harms. Corroboration from outside the benefiting parties is limited to those who find the theoretical arguments compelling, rather than empirical evidence of the problem itself.
narrative_ontology:disappearance_verdict(ai_safety_commitment__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_safety_commitment__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__existential_risk_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.78) because the focus on existential risk often demands substantial redirection of resources, imposes regulatory burdens, and potentially limits future technological progress, representing a significant 'cost' to current and future generations. Suppression is also high (0.70) as the narrative frequently advocates for slowdowns, pauses, or strict controls on AI development, actively suppressing alternative paths. The theater ratio is moderate (0.40) as some efforts might be genuinely functional (e.g., technical alignment research), but others might be performative or serve to legitimize broader control without direct impact on the core, speculative problem. Resistance is high (0.85) due to strong opposition from those prioritizing near-term benefits, different safety paradigms, or rapid AI development.
 *
 * PERSPECTIVAL GAP:
 *   The 'existential risk' reading is experienced very differently by its proponents (existential risk researchers) and its targets (unaligned AI developers, future generations). Proponents see it as a necessary, life-saving coordination effort, while targets may perceive it as an extractive mechanism that stifles innovation or imposes unnecessary costs based on speculative threats. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Existential risk researchers are beneficiaries (d near 0.0) as their work gains prominence and funding, and humanity is the ultimate beneficiary if the risk is averted. Unaligned AI developers and future generations under restrictive governance are targets (d near 1.0) as they bear the costs of regulation, resource redirection, and potentially foregone innovation. AI governance bodies act as agenda-setters, mediating these flows.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Rope (ignoring the significant extraction and suppression) or a pure Snare (ignoring the genuine, albeit speculative, coordination function of preventing extinction). It acknowledges the dual nature: a claimed coordination for humanity's survival, coupled with asymmetric extraction and active enforcement that benefits specific research agendas and governance approaches while imposing costs on others. The 'live' status of the founding problem, combined with high extractiveness, suggests a dynamic where the solution itself may be generating new forms of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    speculative_nature_of_risk,
    'Is the existential risk from misaligned superintelligent AI sufficiently concrete and imminent to warrant the current level of high-cost interventions and resource redirection?',
    'Further empirical evidence on AI capabilities trajectories, breakthroughs in alignment research demonstrating feasibility or impossibility, or a consensus shift within the broader scientific community.',
    'If the risk is deemed less concrete or imminent, the measured extractiveness and suppression might be re-evaluated as disproportionate, potentially reclassifying the constraint towards a Snare. If confirmed as highly concrete and imminent, the Tangled Rope classification would be reinforced, with the extraction seen as a necessary cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speculative_nature_of_risk, empirical, 'Uncertainty regarding the empirical basis and imminence of existential AI risk.').

omega_variable(
    resource_allocation_tradeoffs,
    'Is the intense focus on existential risk diverting critical resources (funding, talent, policy attention) from addressing more immediate, verifiable harms caused by deployed AI systems?',
    'Comprehensive, independent audits of AI safety funding and policy initiatives, comparing resource allocation to the documented prevalence and severity of both speculative and immediate harms.',
    'If significant diversion is confirmed, the ''coordination'' aspect of this constraint would be weakened, and its ''extraction'' aspect (from other safety agendas) would be amplified, potentially pushing it closer to a Snare. If no significant diversion is found, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_tradeoffs, preference, 'Tradeoffs in resource allocation between speculative long-term risks and immediate harms.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (e.g., regulatory mandates, funding biases) or internalized (e.g., fear-driven self-censorship among developers, ideological lock-in)?',
    'Post-policy-change analysis: if AI development patterns and research priorities remain constrained even after formal regulatory pressures are eased, it suggests a significant internalized component of suppression.',
    'If internalized suppression is dominant, the constraint''s effective suppression is higher than the structural measure suggests, as the ''target'' carries the suppression with them. This would amplify the effective extraction for developers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in AI development.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''existential_risk_reading'' of the ''ai_safety_commitment'' kernel. What would be the classification impact if a different reading (e.g., ''near_term_harms_reading'' or ''dual_priority_reading'') were adopted as the primary framing?',
    'Analysis of the structural deltas between readings, as documented in their respective constraint stories and the ''reading_relations'' in cs_structure.',
    'A shift to ''near_term_harms_reading'' would likely result in lower extractiveness and suppression (focusing on accountability rather than slowdowns) and a different victim set. A ''dual_priority_reading'' would likely result in a more balanced, but potentially still extractive, constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Impact of alternative kernel readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__existential_risk_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__existential_risk_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ai_s_tr_t6, ai_safety_commitment__existential_risk_reading, theater_ratio, 6, 0.33).
narrative_ontology:measurement(ai_s_tr_t12, ai_safety_commitment__existential_risk_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(ai_s_tr_t18, ai_safety_commitment__existential_risk_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(ai_s_tr_t24, ai_safety_commitment__existential_risk_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(ai_s_tr_t30, ai_safety_commitment__existential_risk_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__existential_risk_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(ai_s_be_t6, ai_safety_commitment__existential_risk_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(ai_s_be_t12, ai_safety_commitment__existential_risk_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(ai_s_be_t18, ai_safety_commitment__existential_risk_reading, base_extractiveness, 18, 0.74).
narrative_ontology:measurement(ai_s_be_t24, ai_safety_commitment__existential_risk_reading, base_extractiveness, 24, 0.76).
narrative_ontology:measurement(ai_s_be_t30, ai_safety_commitment__existential_risk_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__existential_risk_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ai_s_su_t6, ai_safety_commitment__existential_risk_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(ai_s_su_t12, ai_safety_commitment__existential_risk_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(ai_s_su_t18, ai_safety_commitment__existential_risk_reading, suppression_requirement, 18, 0.68).
narrative_ontology:measurement(ai_s_su_t24, ai_safety_commitment__existential_risk_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement(ai_s_su_t30, ai_safety_commitment__existential_risk_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__existential_risk_reading, global_infrastructure).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, ai_safety_commitment__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, ai_safety_commitment__dual_priority_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'AI safety commitment' kernel. Each reading represents a different structural claim about what AI safety entails, leading to different extractiveness, beneficiaries, and victims. They are linked to show their conceptual and practical interdependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
