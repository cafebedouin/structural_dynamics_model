% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__near_term_harms_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: ai_risk_governance_priority__near_term_harms_reading
 *   human_readable: AI Risk Governance: Prioritizing Near-Term Harms
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the 'AI risk governance
 *   priority' kernel, asserting that the primary focus must be on mitigating
 *   demonstrated present harms (bias, misinformation, labor displacement,
 *   surveillance) affecting marginalized populations. This reading contends
 *   that a focus on speculative existential risks diverts attention and
 *   resources from immediate suffering, effectively benefiting AI developers
 *   and deployers by reducing regulatory pressure on their current
 *   operations. The constraint is classified as a Tangled Rope because it
 *   involves a genuine coordination function (addressing complex, distributed
 *   harms) but also features asymmetric extraction (harms borne by vulnerable
 *   populations, benefits accruing to industry through diverted attention)
 *   and requires active enforcement to shift the governance agenda.
 *
 * KEY AGENTS:
 *   - marginalized_populations: Primary victims (powerless/trapped) – bear direct harms.
 *   - displaced_workers: Victims (moderate/constrained) – face economic disruption.
 *   - global_south_populations: Victims (powerless/trapped) – disproportionately affected by global AI deployments.
 *   - ai_developers: Beneficiaries (powerful/mobile) – benefit from diverted regulatory attention.
 *   - ai_deployers: Beneficiaries (institutional/arbitrage) – profit from less stringent immediate regulation.
 *   - human_rights_advocates: Agenda-setters (organized/constrained) – push for this prioritization.
 *   - x_risk_researchers: Excluded (powerful/identity_locked) – their framing is seen as diverting resources.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, 0.78).
domain_priors:suppression_score(ai_risk_governance_priority__near_term_harms_reading, 0.65).
domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__near_term_harms_reading, "AI Risk Governance: Prioritizing Near-Term Harms").
narrative_ontology:topic_domain(ai_risk_governance_priority__near_term_harms_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__near_term_harms_reading, '55134faf-606a-4842-aace-d29d82e7a1d2').
narrative_ontology:cs_kernel_codification('55134faf-606a-4842-aace-d29d82e7a1d2', distributed).
narrative_ontology:cs_authority_grounding('55134faf-606a-4842-aace-d29d82e7a1d2', distributed).
narrative_ontology:cs_reading_relation('55134faf-606a-4842-aace-d29d82e7a1d2', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('55134faf-606a-4842-aace-d29d82e7a1d2', ai_risk_governance_priority__bridge_reading, coexists_with).
narrative_ontology:cs_axiom('55134faf-606a-4842-aace-d29d82e7a1d2', foundational, present_suffering_demands_immediate_action).
narrative_ontology:cs_axiom_status(present_suffering_demands_immediate_action, holdable).
narrative_ontology:cs_axiom_grounding('55134faf-606a-4842-aace-d29d82e7a1d2', present_suffering_demands_immediate_action, deontological).
narrative_ontology:cs_axiom('55134faf-606a-4842-aace-d29d82e7a1d2', foundational, ai_harms_disproportionately_affect_vulnerable).
narrative_ontology:cs_axiom_status(ai_harms_disproportionately_affect_vulnerable, holdable).
narrative_ontology:cs_axiom_grounding('55134faf-606a-4842-aace-d29d82e7a1d2', ai_harms_disproportionately_affect_vulnerable, empirically_contingent).
narrative_ontology:cs_reference_frame('55134faf-606a-4842-aace-d29d82e7a1d2', human_centered_ai_ethics).
narrative_ontology:cs_drift_state('55134faf-606a-4842-aace-d29d82e7a1d2', contemporary_ai_governance_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('55134faf-606a-4842-aace-d29d82e7a1d2', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, ai_developers).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, ai_deployers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, displaced_workers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, global_south_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the direct and immediate costs of algorithmic bias, surveillance, and misinformation, often with limited recourse or ability to opt-out. Their lives are directly impacted by the deployment of AI systems without adequate safeguards.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, marginalized_populations, payer,
    powerless, immediate, trapped, local).

% Face job losses and deskilling due to automation, often without sufficient retraining or social safety nets. Their economic stability is directly threatened by unchecked AI deployment.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, displaced_workers, payer,
    moderate, biographical, constrained, national).

% Are often targets of exploitative AI applications, data colonialism, and lack the regulatory capacity or infrastructure to mitigate harms from systems developed elsewhere. They bear disproportionate risks with little benefit.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, global_south_populations, payer,
    powerless, generational, trapped, global).

% Benefit from a regulatory environment that, when focused on speculative future risks, diverts attention and resources from immediate, costly compliance for present harms. They can continue rapid deployment with fewer immediate constraints.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, ai_developers, beneficiary,
    powerful, biographical, mobile, global).

% Profit from the widespread deployment of AI systems, often without fully internalizing the social costs of bias, misinformation, or labor displacement. A focus on long-term, speculative risks reduces immediate regulatory pressure on their business models.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, ai_deployers, beneficiary,
    institutional, biographical, arbitrage, global).

% Actively push for policies and regulations that address present, demonstrable harms of AI. They coordinate efforts to document harms, lobby policymakers, and raise public awareness, seeking to shift the governance agenda.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, human_rights_advocates, agenda_setter,
    organized, generational, constrained, global).

% Focus on preventing catastrophic or existential risks from advanced AI. From the perspective of near-term harms, their framing is seen as diverting resources and attention from immediate suffering, effectively excluding the voices of those currently harmed.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, x_risk_researchers, excluded,
    powerful, civilizational, identity_locked, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate global and national efforts to identify, measure, and mitigate the tangible, immediate negative impacts of AI technologies on human rights, social equity, and economic stability.
% TRANSFER_FUNCTION: Seeks to transfer regulatory attention, research funding, and policy implementation resources from speculative, long-term AI risks to the demonstrable harms currently experienced by vulnerable populations, thereby shifting the burden of harm mitigation onto AI developers and deployers.
% ABSENT_VOICES: The voices of those advocating solely for existential risk mitigation are structurally sidelined in this framing, as their concerns are deemed less urgent or directly relevant to the immediate suffering caused by current AI deployments. Their perspective is seen as diverting resources from present-day victims.
% DISAPPEARANCE_RATIONALE: If the prioritization of near-term harms vanished, the current harms would likely intensify without focused mitigation efforts. AI development would continue with less accountability for social impact, and marginalized populations would bear even greater costs. The global AI governance discourse would shift back towards more abstract, long-term concerns.
% FOUNDING_PROBLEM: The rapid deployment of AI systems without adequate ethical oversight or regulatory frameworks led to demonstrable harms such as algorithmic bias, job displacement, and privacy violations, disproportionately affecting vulnerable groups.
% FOUNDING_PROBLEM_CORROBORATION: Numerous reports from human rights organizations, academic studies on algorithmic bias, and testimonies from affected communities corroborate the ongoing nature and severity of these harms. These sources are independent of the AI developers and deployers who benefit from a less stringent regulatory focus on present harms.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__near_term_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_risk_governance_priority__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__near_term_harms_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the ongoing and disproportionate harms borne by vulnerable populations due to current AI deployments, while the benefits of a less regulated environment accrue to industry. Suppression (0.65) is significant, as powerful industry lobbies and the allure of 'frontier AI' often resist stringent regulation of present harms. The theater ratio (0.20) is relatively low, as the advocacy for near-term harms is a direct and functional effort, not primarily performative. The rising extractiveness and suppression over the interval reflect the increasing scale of AI deployment and the ongoing struggle to implement effective mitigation.
 *
 * PERSPECTIVAL GAP:
 *   AI developers and deployers perceive this constraint as an unnecessary burden that could stifle innovation, while marginalized populations and human rights advocates see it as a critical, overdue reorientation of priorities. The engine's classification will highlight this divergence: a claimed 'tangled_rope' from the perspective of those advocating for near-term harms, but potentially a 'snare' from the perspective of the victims, and a 'rope' or even 'mountain' from the perspective of those who benefit from the status quo.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized populations, displaced workers, and Global South populations are clear targets (high d) as they bear the direct costs. AI developers and deployers are beneficiaries (low d) because a focus on speculative x-risk reduces immediate regulatory pressure on their profitable, present-day activities. Human rights advocates are agenda-setters, pushing for this prioritization, and x-risk researchers are structurally excluded from this specific framing, as their concerns are seen as diverting attention from immediate suffering.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading implicitly argues that the 'mandate' of AI risk governance has drifted from its original, implicit function of protecting human well-being in the face of new technology, towards a more abstract and less accountable focus on speculative future scenarios. By prioritizing present harms, it seeks to resolve this mandatrophy by re-centering governance on demonstrable, measurable impacts and holding current actors accountable, preventing the mislabeling of industry benefit as 'necessary innovation' or 'long-term safety'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_diversion_impact,
    'To what extent does a primary focus on near-term harms genuinely divert resources from existential risk mitigation, versus simply reallocating them within the broader AI safety budget?',
    'Empirical analysis of funding flows and policy initiatives in AI governance, tracking the actual allocation of resources between different risk categories over time.',
    'If diversion is significant, this reading''s claim of ''benefiting AI developers'' by reducing regulatory pressure on present harms is strengthened. If reallocation is minimal, the readings may be more ''coexisting'' than ''influencing'' in terms of resource competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_diversion_impact, empirical, 'Assesses the actual resource competition between near-term and existential AI risk mitigation efforts.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a distinct reading of the ''ai_risk_governance_priority'' kernel, or is it merely a policy preference within a broader, unified framework?',
    'Analyze the foundational axioms and proposed policy mechanisms of this reading against its siblings. If the core normative claims and proposed interventions are mutually exclusive or fundamentally incompatible within a single coherent governance framework, it is a distinct reading.',
    'If it''s a distinct reading, the framework''s ability to model competing normative commitments is validated. If it''s a mere policy preference, the kernel decomposition may be over-specified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms this constraint as a distinct reading of the AI risk governance priority kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of near-term harm mitigation structural (e.g., lack of regulatory power, industry lobbying) or internalized (e.g., cognitive biases favoring innovation over safety, ''move fast and break things'' culture)?',
    'Post-policy suppression trajectory: if suppression of harm mitigation persists after structural barriers are removed (e.g., new regulations are passed), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the resistance to addressing harms is embedded within the industry''s culture and practices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in AI harm mitigation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__near_term_harms_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 10, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__near_term_harms_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_risk_governance_priority' kernel, focusing on near-term harms. It is structurally distinct from the 'existential_risk_reading' and 'bridge_reading' of the same kernel, which prioritize different aspects of AI risk.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
