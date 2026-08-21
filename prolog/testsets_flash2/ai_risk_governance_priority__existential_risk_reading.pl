% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: ai_risk_governance_priority__existential_risk_reading
 *   human_readable: AI Risk Governance: Existential Risk Prioritization
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'existential risk' reading of AI
 *   governance, where preventing superintelligence scenarios that could
 *   annihilate or permanently curtail humanity's potential is the paramount
 *   priority. This framing directs significant resources and policy attention
 *   towards highly speculative future risks, often at the expense of
 *   addressing immediate, demonstrable harms caused by AI systems. The
 *   constraint is claimed as a 'tangled_rope' because it genuinely
 *   coordinates some long-term safety efforts while simultaneously extracting
 *   resources and attention from other critical areas, benefiting specific
 *   institutions and labs.
 *
 * KEY AGENTS:
 *   - x_risk_research_institutions: Primary agenda-setter (institutional/mobile) — defines the problem, directs resources.
 *   - ai_labs_claiming_safety_leadership: Primary beneficiary (powerful/arbitrage) — benefits from the narrative, deflects scrutiny.
 *   - near_term_harms_advocates: Primary payer (organized/constrained) — bears the cost of deprioritization.
 *   - marginalized_communities_affected_by_ai: Primary victim (powerless/trapped) — directly harmed by unmitigated present risks.
 *   - policy_makers: Agenda-setter (institutional/constrained) — allocates resources based on perceived priorities.
 *   - general_public: Beneficiary/Payer (moderate/constrained) — theoretically protected from future risks, but bears present costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, 0.65).
domain_priors:suppression_score(ai_risk_governance_priority__existential_risk_reading, 0.4).
domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "AI Risk Governance: Existential Risk Prioritization").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, 'cc7f6575-b3d0-4f13-86bd-331edc32a93f').
narrative_ontology:cs_kernel_codification('cc7f6575-b3d0-4f13-86bd-331edc32a93f', distributed).
narrative_ontology:cs_authority_grounding('cc7f6575-b3d0-4f13-86bd-331edc32a93f', extraction).
narrative_ontology:cs_interpretation_layer_present('cc7f6575-b3d0-4f13-86bd-331edc32a93f').
narrative_ontology:cs_reading_relation('cc7f6575-b3d0-4f13-86bd-331edc32a93f', ai_risk_governance_priority__near_term_harms_reading, influences).
narrative_ontology:cs_reading_relation('cc7f6575-b3d0-4f13-86bd-331edc32a93f', ai_risk_governance_priority__bridge_reading, coexists_with).
narrative_ontology:cs_axiom('cc7f6575-b3d0-4f13-86bd-331edc32a93f', foundational, existential_risk_is_paramount).
narrative_ontology:cs_axiom_status(existential_risk_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('cc7f6575-b3d0-4f13-86bd-331edc32a93f', existential_risk_is_paramount, deontological).
narrative_ontology:cs_axiom('cc7f6575-b3d0-4f13-86bd-331edc32a93f', foundational, superintelligence_is_imminent_and_uncontrollable).
narrative_ontology:cs_axiom_status(superintelligence_is_imminent_and_uncontrollable, holdable).
narrative_ontology:cs_axiom_grounding('cc7f6575-b3d0-4f13-86bd-331edc32a93f', superintelligence_is_imminent_and_uncontrollable, empirically_contingent).
narrative_ontology:cs_reference_frame('cc7f6575-b3d0-4f13-86bd-331edc32a93f', humanity_at_risk_from_agi).
narrative_ontology:cs_drift_state('cc7f6575-b3d0-4f13-86bd-331edc32a93f', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cc7f6575-b3d0-4f13-86bd-331edc32a93f', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, near_term_harms_advocates).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, marginalized_communities_affected_by_ai).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, general_public).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions define the scope of 'AI risk' primarily around long-term, catastrophic scenarios. They receive significant funding and influence policy discussions, directing resources towards alignment-as-control and AGI governance frameworks. Their focus on speculative future risks often de-emphasizes present-day harms.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions, agenda_setter,
    institutional, civilizational, mobile, global).

% Major AI development companies benefit from this prioritization by framing their advanced research as 'safety-critical' and positioning themselves as essential for preventing existential risks. This narrative can deflect scrutiny from current product harms and consolidate power in the hands of a few large players.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership, beneficiary,
    powerful, generational, arbitrage, global).

% Advocates for addressing immediate harms (bias, misinformation, labor displacement) find their concerns deprioritized and underfunded when existential risk dominates the governance agenda. They bear the cost of diverted attention and resources, struggling to implement policies for present-day issues.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, near_term_harms_advocates, payer,
    organized, biographical, constrained, national).

% These communities experience the direct, negative impacts of AI systems (e.g., biased algorithms in policing, credit, or healthcare). Their urgent needs are often overlooked in favor of abstract, future-oriented risk discussions, leaving them to bear the brunt of unmitigated present harms.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, marginalized_communities_affected_by_ai, payer,
    powerless, immediate, trapped, local).

% Government bodies and international organizations tasked with AI governance are influenced by the dominant risk narrative. They allocate funding and legislative attention based on perceived priorities, often balancing competing demands but leaning towards the most vocal or well-resourced advocacy.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% The public is theoretically protected from catastrophic future risks, which is presented as a benefit. However, they also bear the costs of unaddressed present harms and the potential for concentrated power in AI development, without direct agency in the prioritization debate.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, general_public, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, general_public, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global efforts and resources towards a unified goal of preventing catastrophic AI outcomes, ensuring that humanity's long-term future is secured against highly advanced AI systems.
% TRANSFER_FUNCTION: Transfers significant financial, intellectual, and political capital from addressing immediate, tangible AI harms to speculative, long-term existential risks, from present-day affected communities to institutions focused on future-oriented AI safety research.
% ABSENT_VOICES: Advocates for near-term harms, especially those representing marginalized communities, are often sidelined or their concerns reframed as secondary. Their voices would emphasize the urgency of current injustices and the need for equitable AI development.
% DISAPPEARANCE_RATIONALE: If this prioritization vanished, resources would immediately reallocate towards mitigating present-day harms, and the focus of AI safety research would broaden to include more immediate, verifiable risks. The current institutional landscape of AI governance would undergo significant restructuring.
% FOUNDING_PROBLEM: The potential for advanced AI systems to develop capabilities beyond human control, leading to unintended and catastrophic consequences for humanity's long-term future.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of existential risk prioritization (e.g., leading AI researchers, philosophers, and dedicated research institutes) attest that the problem is live and growing. Critics (e.g., ethicists, social scientists, and civil society organizations) acknowledge the theoretical possibility but argue its urgency is overblown relative to present harms, making the status 'contested'.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_risk_governance_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__existential_risk_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) reflects the significant diversion of resources and attention from present harms to speculative future risks. Suppression (0.40) is moderate, as advocates for near-term harms are not entirely silenced but are often marginalized in policy debates. The theater ratio (0.55) is high because a substantial portion of 'safety' work under this framing is performative, focusing on abstract alignment problems rather than verifiable, real-world impacts. The claimed type is 'tangled_rope' because there is a genuine coordination function (mobilizing against a perceived catastrophic threat) but it is coupled with asymmetric extraction and requires active enforcement (e.g., through funding mechanisms and policy influence) to maintain its dominance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of x-risk institutions, this is a vital 'rope' coordinating humanity's defense against an existential threat. From the perspective of near-term harms advocates and affected communities, it functions as a 'snare' or 'tangled_rope', diverting resources and attention from their urgent needs under the guise of future safety. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk research institutions and AI labs claiming safety leadership are clear beneficiaries (low d) as they gain funding, influence, and a favorable public narrative. Near-term harms advocates and marginalized communities are victims/payers (high d) as their issues are deprioritized and they bear the unmitigated costs of present AI harms. Policy makers are agenda-setters, balancing competing interests but often swayed by the powerful narrative of existential risk.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure 'rope' (genuine coordination) by highlighting the significant extraction and suppression involved. It also avoids mislabeling it as a pure 'snare' by acknowledging the genuine, albeit contested, coordination function around long-term AI safety. The 'tangled_rope' classification captures the hybrid nature, where a legitimate concern is leveraged to create an extractive structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_efficiency,
    'Is the current allocation of AI governance resources, heavily skewed towards existential risk, the most efficient way to mitigate the full spectrum of AI-related risks (both near-term and long-term)?',
    'Comprehensive, independent cost-benefit analysis comparing the societal impact of mitigating present harms versus preventing speculative future risks, considering opportunity costs.',
    'If inefficient, it would strengthen the case for reallocating resources, potentially shifting the constraint towards a ''snare'' or ''piton'' if the coordination function is found to be minimal relative to extraction. If efficient, it would support the ''rope'' aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, empirical, 'Efficiency of resource allocation under existential risk prioritization.').

omega_variable(
    speculative_vs_demonstrated_harm,
    'What is the appropriate epistemic weighting between speculative, high-impact future risks and demonstrated, lower-impact present harms in AI governance prioritization?',
    'Development of a robust, interdisciplinary risk assessment framework that integrates both probabilistic future scenarios and empirical evidence of current societal impacts, with transparent weighting criteria.',
    'A shift towards higher weighting for demonstrated harms would reduce the perceived legitimacy of this prioritization, potentially reclassifying it as a ''snare'' from the perspective of affected communities. A continued emphasis on speculative risks would reinforce its current classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(speculative_vs_demonstrated_harm, conceptual, 'Epistemic weighting of speculative vs. demonstrated AI harms.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of near-term harms advocacy structural (e.g., funding biases, institutional gatekeeping) or internalized (e.g., self-censorship by researchers fearing reputational damage)?',
    'Post-exit suppression trajectory: if advocacy for near-term harms gains significant traction and funding after a shift in institutional priorities, it suggests structural suppression. If it remains marginalized despite structural changes, internalized suppression is more prominent.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — advocates carry the suppression with them after structural barriers are removed. If structural, removing barriers would more directly empower alternative framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for near-term harms advocacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 10, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'AI risk governance priority' kernel. This 'existential_risk_reading' prioritizes long-term, catastrophic AI scenarios. It influences the 'near_term_harms_reading' by diverting resources and attention, and coexists with the 'bridge_reading' which seeks to integrate both perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
