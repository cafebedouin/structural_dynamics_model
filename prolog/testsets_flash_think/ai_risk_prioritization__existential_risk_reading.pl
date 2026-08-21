% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__existential_risk_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: ai_risk_prioritization__existential_risk_reading
 *   human_readable: AI Risk Prioritization: Existential Threat Reading
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the prioritization of AI risk as primarily
 *   existential, focusing on the potential for misaligned AGI to cause
 *   extinction-level threats. It frames alignment research and capability
 *   controls as paramount, often at the expense of addressing more immediate,
 *   measurable harms from deployed AI systems. This is one reading of the
 *   broader 'ai_risk_prioritization' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, 0.65).
domain_priors:suppression_score(ai_risk_prioritization__existential_risk_reading, 0.78).
domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__existential_risk_reading, "AI Risk Prioritization: Existential Threat Reading").
narrative_ontology:topic_domain(ai_risk_prioritization__existential_risk_reading, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__existential_risk_reading, '5a303c9f-1250-46da-a533-48a54d41bffe').
narrative_ontology:cs_kernel_codification('5a303c9f-1250-46da-a533-48a54d41bffe', implicit).
narrative_ontology:cs_authority_grounding('5a303c9f-1250-46da-a533-48a54d41bffe', expertise).
narrative_ontology:cs_interpretation_layer_present('5a303c9f-1250-46da-a533-48a54d41bffe').
narrative_ontology:cs_reading_relation('5a303c9f-1250-46da-a533-48a54d41bffe', ai_risk_prioritization__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('5a303c9f-1250-46da-a533-48a54d41bffe', foundational, agi_extinction_risk_primary).
narrative_ontology:cs_axiom_status(agi_extinction_risk_primary, holdable).
narrative_ontology:cs_axiom_grounding('5a303c9f-1250-46da-a533-48a54d41bffe', agi_extinction_risk_primary, empirically_contingent).
narrative_ontology:cs_reference_frame('5a303c9f-1250-46da-a533-48a54d41bffe', precautionary_principle_for_agi).
narrative_ontology:cs_drift_state('5a303c9f-1250-46da-a533-48a54d41bffe', contemporary_ai_development, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5a303c9f-1250-46da-a533-48a54d41bffe', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, longtermist_funders).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, future_humanity).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_ai_harms_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, ai_developers_and_researchers).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, ai_developers_and_researchers).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__existential_risk_reading, longtermism_doctrine).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__existential_risk_reading, effective_altruism_principles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions define the problem of AGI existential risk, conduct alignment research, and advocate for policy interventions. They receive significant funding and influence policy agendas based on this prioritization.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Provide substantial financial backing to x-risk research and advocacy, aligning their philanthropic strategies with the existential risk framing. Their influence is amplified by the perceived urgency and scale of the threat.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, longtermist_funders, beneficiary,
    powerful, civilizational, arbitrage, global).

% The ultimate beneficiaries of successful alignment, but also the primary victims of the existential threat this constraint seeks to avert. They bear the hypothetical cost of extinction, and their interests are represented by current advocates.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).

% Advocate for addressing immediate harms from AI, such as bias, discrimination, labor displacement, and surveillance. Their concerns are often framed as secondary or distracting from the existential priority, leading to reduced funding and policy attention for their work.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, near_term_ai_harms_advocates, excluded,
    organized, biographical, constrained, global).

% Are pressured to integrate alignment considerations into their work and to prioritize safety over capabilities. Those whose work aligns with x-risk funding benefit; others find their research directions deprioritized or unfunded. Their exit options are constrained by funding priorities and the dominant narrative.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, ai_developers_and_researchers, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, ai_developers_and_researchers, beneficiary).

% Are influenced by the existential risk narrative to prioritize AGI safety and capability controls in legislation and regulation, potentially diverting resources from other AI governance concerns. They act as gatekeepers for policy implementation.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, policy_makers, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global scientific, policy, and philanthropic efforts towards preventing an existential catastrophe from misaligned artificial general intelligence (AGI).
% TRANSFER_FUNCTION: Transfers significant financial resources, research talent, and policy attention from addressing near-term, measurable AI harms (e.g., bias, labor displacement) to long-term, speculative existential risk mitigation.
% ABSENT_VOICES: Victims of current AI harms, advocates for algorithmic justice, and researchers focused on immediate societal impacts of AI are often marginalized or excluded from high-level policy discussions dominated by the existential risk framing. They would argue for a more balanced or inverted prioritization.
% DISAPPEARANCE_RATIONALE: If the prioritization of existential AI risk vanished overnight, funding and policy focus would immediately reallocate to other AI-related concerns, particularly near-term harms. Research institutions dedicated to x-risk would lose their primary mandate, and the global AI governance agenda would fundamentally shift.
% FOUNDING_PROBLEM: The perceived catastrophic risk of unaligned superintelligent AI leading to human extinction or irreversible disempowerment.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by a significant portion of the AI safety research community, prominent public intellectuals, and some technology leaders. Critics, particularly those focused on near-term harms, contest its primacy and urgency, arguing that it distracts from present dangers.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_risk_prioritization__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__existential_risk_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_prioritization__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is high because this prioritization diverts significant resources and attention from other pressing AI-related issues, effectively extracting from those concerns. Suppression (0.78) is also high, as alternative framings (e.g., near-term harms) are actively marginalized or dismissed as distractions. Theater ratio (0.20) is relatively low, as the core alignment research is genuinely pursued, though some advocacy may be performative. Accessibility collapse (0.70) is high because the dominant narrative makes it difficult for alternative risk framings to gain traction or funding. Resistance (0.45) is moderate, as there is an organized counter-movement, but it struggles against the powerful existential narrative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of x-risk research institutions and longtermist funders, this constraint is a vital coordination mechanism to save humanity. From the perspective of near-term harms advocates, it is an extractive mechanism that suppresses urgent, tangible problems in favor of speculative, distant ones. The engine will compute these divergent classifications based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk research institutions and longtermist funders are clear beneficiaries, gaining influence, funding, and agenda-setting power. Future humanity is a victim, as they are the target of the threat, and their interests are represented by the constraint's proponents. Near-term AI harms advocates are also victims, as their concerns are deprioritized and suppressed. AI developers and policymakers are both payers (of attention/resources to x-risk) and potential beneficiaries (if their work aligns with the dominant agenda).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the constraint as a pure Rope (ignoring the extraction from near-term concerns) or a pure Snare (ignoring the genuine coordination function of addressing a perceived existential threat). It highlights the dual nature: coordinating against a perceived threat while simultaneously extracting from alternative risk framings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_threat_empirical_status,
    'Is the existential threat from misaligned AGI an empirically verifiable risk, a speculative projection, or a conceptual possibility?',
    'Development of AGI and subsequent empirical observation of alignment challenges, or a consensus shift in the scientific community regarding the probability and nature of the threat.',
    'If empirically disproven or significantly downgraded, the constraint''s justification would collapse, reducing its extractiveness and suppression. If confirmed, its coordination function would be strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_threat_empirical_status, empirical, 'The empirical status of the AGI existential threat.').

omega_variable(
    suppression_of_near_term_harms,
    'Is the suppression of near-term AI harms advocacy a necessary trade-off for existential risk mitigation, or an extractive side-effect of resource capture?',
    'Analysis of resource allocation and policy outcomes: if significant resources are diverted from demonstrable harms without clear evidence of existential threat reduction, it points to extractive side-effects.',
    'If deemed an extractive side-effect, the constraint''s effective extraction would be higher, and its coordination function would be seen as more compromised by rent-seeking behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_near_term_harms, conceptual, 'Whether deprioritizing near-term harms is a necessary cost or an unjustified extraction.').

omega_variable(
    kernel_reading_disagreement_locus,
    'This constraint is one reading of the ''ai_risk_prioritization'' kernel. The sibling reading is ''near_term_harms_reading''. Where is the core disagreement located?',
    'Conceptual analysis of the foundational axioms and reference frames of both readings.',
    'Understanding the locus of disagreement (e.g., timescale, scope of ''harm'', definition of ''risk'') clarifies the structural tension between the readings and informs potential pathways for resolution or reconciliation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locus of disagreement between existential and near-term AI risk readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__existential_risk_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__existential_risk_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_prioritization__existential_risk_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_prioritization__existential_risk_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_prioritization__existential_risk_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(ai_r_tr_t16, ai_risk_prioritization__existential_risk_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_prioritization__existential_risk_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(ai_r_be_t16, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 4, 0.68).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 8, 0.72).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 12, 0.75).
narrative_ontology:measurement(ai_r_su_t16, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 16, 0.78).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__existential_risk_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_ethics_governance).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_research_funding).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_capability_controls).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
