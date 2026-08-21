% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__ethics_justice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__ethics_justice_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: ai_alignment_commitment__ethics_justice_reading
 *   human_readable: AI Alignment: Preventing Social Bias and Present-Day Harm (Ethics & Justice Reading)
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'ethics and justice' reading of AI
 *   alignment, which prioritizes preventing the reproduction of social biases
 *   and present-day harms in AI systems. It focuses on the immediate,
 *   demonstrable impact of AI on marginalized populations. This reading is
 *   one of several competing interpretations of 'AI alignment,' each with
 *   different beneficiaries, victims, and resource allocations. The metrics
 *   reflect the costs imposed on AI developers and long-term safety
 *   researchers, and the active enforcement required to shift focus towards
 *   ethical considerations.
 *
 * KEY AGENTS:
 *   - marginalized_communities: Primary beneficiaries (organized/constrained)
 *   - ethics_researchers: Agenda-setters (moderate/mobile)
 *   - ai_developers_deployers: Primary payers (institutional/constrained)
 *   - long_term_safety_researchers: Secondary payers (moderate/constrained)
 *   - policy_makers: Agenda-setters (institutional/mobile)
 *   - general_public: Beneficiaries (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, 0.65).
domain_priors:suppression_score(ai_alignment_commitment__ethics_justice_reading, 0.45).
domain_priors:theater_ratio(ai_alignment_commitment__ethics_justice_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__ethics_justice_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__ethics_justice_reading, "AI Alignment: Preventing Social Bias and Present-Day Harm (Ethics & Justice Reading)").
narrative_ontology:topic_domain(ai_alignment_commitment__ethics_justice_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_commitment__ethics_justice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__ethics_justice_reading, '950cd0b3-6c59-470c-8cdf-f0333b774a71').
narrative_ontology:cs_kernel_codification('950cd0b3-6c59-470c-8cdf-f0333b774a71', distributed).
narrative_ontology:cs_authority_grounding('950cd0b3-6c59-470c-8cdf-f0333b774a71', practice).
narrative_ontology:cs_interpretation_layer_present('950cd0b3-6c59-470c-8cdf-f0333b774a71').
narrative_ontology:cs_reading_relation('950cd0b3-6c59-470c-8cdf-f0333b774a71', ai_alignment_commitment__safety_control_reading, influences).
narrative_ontology:cs_reading_relation('950cd0b3-6c59-470c-8cdf-f0333b774a71', ai_alignment_commitment__integrated_reading, coexists_with).
narrative_ontology:cs_axiom('950cd0b3-6c59-470c-8cdf-f0333b774a71', foundational, ai_must_not_reproduce_social_bias).
narrative_ontology:cs_axiom_status(ai_must_not_reproduce_social_bias, holdable).
narrative_ontology:cs_axiom_grounding('950cd0b3-6c59-470c-8cdf-f0333b774a71', ai_must_not_reproduce_social_bias, deontological).
narrative_ontology:cs_axiom('950cd0b3-6c59-470c-8cdf-f0333b774a71', foundational, present_day_harms_are_primary_concern).
narrative_ontology:cs_axiom_status(present_day_harms_are_primary_concern, holdable).
narrative_ontology:cs_axiom_grounding('950cd0b3-6c59-470c-8cdf-f0333b774a71', present_day_harms_are_primary_concern, instrumental).
narrative_ontology:cs_reference_frame('950cd0b3-6c59-470c-8cdf-f0333b774a71', equitable_ai_development_framework).
narrative_ontology:cs_drift_state('950cd0b3-6c59-470c-8cdf-f0333b774a71', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('950cd0b3-6c59-470c-8cdf-f0333b774a71', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, ethics_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, ai_developers_deployers).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities are the primary beneficiaries, as the constraint aims to mitigate and prevent the reproduction of historical and present-day biases and harms that disproportionately affect them through AI systems. Their 'exit' is often through collective action and advocacy.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, marginalized_communities, beneficiary,
    organized, generational, constrained, global).

% These researchers define, advocate for, and develop methods to implement the principles of fairness, accountability, and transparency in AI. They gain influence and resources as this reading of alignment gains prominence. Their mobility comes from the demand for their expertise across various institutions.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ethics_researchers, agenda_setter,
    moderate, biographical, mobile, global).

% Bear the costs of implementing bias detection, mitigation, and fairness audits, which can slow development, increase costs, and require re-engineering systems. Their exit options are constrained by market pressures and regulatory expectations to address these issues.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_developers_deployers, payer,
    institutional, immediate, constrained, global).

% Experience a diversion of resources and attention away from their focus on existential and catastrophic risks from advanced AI, towards immediate social harms. Their work may be de-prioritized or reframed as less urgent. Their exit is constrained by the specialized nature of their field.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers, payer,
    moderate, generational, constrained, global).

% Are influenced by this reading to create regulations and guidelines that mandate fairness and bias mitigation in AI, often in response to public pressure and advocacy from marginalized communities. They can shift policy focus based on perceived urgency and political will.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, policy_makers, agenda_setter,
    institutional, generational, mobile, national).

% Benefits from AI systems that are less biased and less likely to cause harm, leading to more equitable outcomes in areas like hiring, lending, and criminal justice. However, they are largely passive recipients of these benefits and have limited direct influence over the constraint's implementation.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, general_public, beneficiary,
    powerless, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates efforts across AI development, deployment, and governance to identify, measure, and mitigate social biases and present-day harms, ensuring AI systems are developed and used equitably.
% TRANSFER_FUNCTION: Transfers resources (funding, developer time, research focus) from purely technical optimization or long-term speculative risks towards immediate ethical and social impact assessments, fairness audits, and community engagement.
% ABSENT_VOICES: AI developers focused solely on performance metrics or rapid deployment, who might argue that ethical considerations unduly slow innovation, are often marginalized in discussions prioritizing this reading of alignment. Their concerns are often framed as secondary to immediate harm prevention.
% DISAPPEARANCE_RATIONALE: If this commitment vanished, AI development would likely revert to prioritizing efficiency and profit without robust ethical safeguards, leading to an exacerbation of social biases and harms, particularly for vulnerable populations. Advocacy groups would lose a key lever for accountability.
% FOUNDING_PROBLEM: The rapid deployment of AI systems began to reproduce and amplify existing social biases, leading to discriminatory outcomes in critical domains like hiring, credit, and criminal justice, causing tangible harm to marginalized communities.
% FOUNDING_PROBLEM_CORROBORATION: Numerous academic studies, investigative journalism reports, and testimonies from affected communities consistently corroborate the ongoing reproduction of social bias and present-day harm by AI systems. These sources are independent of the ethics researchers who directly benefit from this focus.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__ethics_justice_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__ethics_justice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__ethics_justice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_alignment_commitment__ethics_justice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__ethics_justice_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__ethics_justice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__ethics_justice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because it demands significant re-prioritization and resource allocation from AI developers and long-term safety researchers. Suppression (0.45) is moderate, reflecting the active advocacy and regulatory pressure needed to enforce this focus against competing priorities. Theater ratio (0.20) is low, as the commitment to addressing bias and harm is largely genuine, though some 'ethics washing' may occur. Resistance (0.70) is high, as there are strong counter-pressures from those who bear the costs.
 *
 * PERSPECTIVAL GAP:
 *   AI developers and long-term safety researchers experience this as an extractive constraint, diverting resources and attention from their core objectives. Marginalized communities and ethics researchers, however, perceive it as a necessary coordination mechanism to ensure equitable and just AI development. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized communities and ethics researchers are beneficiaries (low d) as the constraint directly addresses their concerns and elevates their expertise. AI developers/deployers and long-term safety researchers are targets (high d) as they bear the costs of implementation and re-prioritization. Policy makers are agenda-setters, mediating between these groups.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of alignment is far from mandatrophy; its mandate is actively growing in response to observed harms. The classification as a Tangled Rope reflects the genuine coordination function (protecting vulnerable populations) intertwined with asymmetric extraction (from developers and other research priorities). It prevents mislabeling as a Snare by acknowledging the real coordination problem it addresses, while also highlighting the costs it imposes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_tradeoff,
    'To what extent does prioritizing present-day harms divert critical resources from long-term catastrophic risk prevention, and what are the net societal consequences?',
    'Comprehensive, independent economic and risk modeling that quantifies the opportunity costs and comparative benefits of different alignment priorities over various time horizons.',
    'If the diversion is found to significantly increase catastrophic risk without commensurate reduction in present-day harm, the extractiveness from long-term safety researchers would be re-evaluated as more severe, potentially shifting the constraint towards a Snare for that seat. Conversely, if present-day harms are found to be a prerequisite for addressing long-term risks, the extraction would be seen as a necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_tradeoff, empirical, 'The irreducible uncertainty regarding the optimal allocation of resources between immediate ethical concerns and speculative long-term AI safety risks.').

omega_variable(
    scope_of_harm_definition,
    'Is the definition of ''harm'' sufficiently broad to capture all relevant negative impacts, or does its focus on social bias inadvertently exclude other forms of present-day harm or future risks?',
    'Longitudinal studies tracking the full spectrum of AI impacts, including environmental, geopolitical, and psychological effects, alongside social bias metrics. Expert consensus on a multi-dimensional harm taxonomy.',
    'If the definition is too narrow, the constraint''s effectiveness in achieving true alignment would be reduced, and its claimed benefits might be overstated, potentially increasing the theater_ratio. If it''s appropriately broad, its coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_harm_definition, conceptual, 'Ambiguity in the scope and definition of ''harm'' within the ethics and justice reading of AI alignment.').

omega_variable(
    ethics_washing_prevalence,
    'What proportion of ''ethics and justice'' initiatives are genuine attempts to mitigate harm versus performative ''ethics washing'' designed to improve public image or preempt regulation?',
    'Independent audits of AI ethics programs, tracking measurable outcomes in bias reduction and harm prevention, rather than relying on self-reported compliance or policy declarations. Analysis of resource allocation within companies to ethics teams vs. core development.',
    'A high prevalence of ethics washing would significantly increase the theater_ratio, potentially reclassifying the constraint towards a Piton or even a Snare if the performative aspects are primarily extractive from public trust or regulatory attention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ethics_washing_prevalence, empirical, 'Uncertainty regarding the true intent and efficacy of AI ethics initiatives versus their use for public relations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__ethics_justice_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 15, 0.43).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 20, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__ethics_justice_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_governance_frameworks).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_development_standards).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'AI alignment commitment' kernel. This 'ethics_justice_reading' focuses on social bias and present-day harm, while 'safety_control_reading' focuses on catastrophic risks, and 'integrated_reading' attempts to combine both. Each is a distinct constraint with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
