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
 *   constraint_id: ai_risk_prioritization__existential_risk_reading
 *   human_readable: AI Risk Prioritization: Existential Risk Reading
 *   domain: AI Safety/Technology Governance/Risk Assessment
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, 0.65).
domain_priors:suppression_score(ai_risk_prioritization__existential_risk_reading, 0.7).
domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__existential_risk_reading, "AI Risk Prioritization: Existential Risk Reading").
narrative_ontology:topic_domain(ai_risk_prioritization__existential_risk_reading, "AI Safety/Technology Governance/Risk Assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__existential_risk_reading, 'd802afe5-a2ed-4e3b-a886-6ca4ed64114f').
narrative_ontology:cs_kernel_codification('d802afe5-a2ed-4e3b-a886-6ca4ed64114f', distributed).
narrative_ontology:cs_authority_grounding('d802afe5-a2ed-4e3b-a886-6ca4ed64114f', expertise).
narrative_ontology:cs_interpretation_layer_present('d802afe5-a2ed-4e3b-a886-6ca4ed64114f').
narrative_ontology:cs_reading_relation('d802afe5-a2ed-4e3b-a886-6ca4ed64114f', ai_risk_prioritization__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('d802afe5-a2ed-4e3b-a886-6ca4ed64114f', foundational, agi_extinction_is_terminal_risk).
narrative_ontology:cs_axiom_status(agi_extinction_is_terminal_risk, holdable).
narrative_ontology:cs_axiom_grounding('d802afe5-a2ed-4e3b-a886-6ca4ed64114f', agi_extinction_is_terminal_risk, deontological).
narrative_ontology:cs_axiom('d802afe5-a2ed-4e3b-a886-6ca4ed64114f', foundational, alignment_is_solvable_technical_problem).
narrative_ontology:cs_axiom_status(alignment_is_solvable_technical_problem, holdable).
narrative_ontology:cs_axiom_grounding('d802afe5-a2ed-4e3b-a886-6ca4ed64114f', alignment_is_solvable_technical_problem, empirically_contingent).
narrative_ontology:cs_reference_frame('d802afe5-a2ed-4e3b-a886-6ca4ed64114f', agi_existential_threat_paradigm).
narrative_ontology:cs_drift_state('d802afe5-a2ed-4e3b-a886-6ca4ed64114f', contemporary_ai_governance_debate, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d802afe5-a2ed-4e3b-a886-6ca4ed64114f', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, longtermist_funders).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, future_humanity).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_ai_harms_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, ai_developers_and_corporations).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, ai_developers_and_corporations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions define the research agenda, secure funding, and advocate for policies prioritizing AGI alignment and capability controls, framing these as the most critical interventions for AI safety. They benefit from the allocation of resources and attention to their specific research focus.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Provide significant financial backing to x-risk research institutions and related advocacy efforts. Their philanthropic and investment strategies are aligned with the prioritization of long-term existential risks, seeing their contributions as safeguarding the future of humanity.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, longtermist_funders, beneficiary,
    powerful, civilizational, mobile, global).

% The ultimate 'victim' of unaligned AGI, bearing the extinction-level threat. This group includes non-existent persons whose potential existence is at stake. They have no agency or voice in current debates, and their interests are represented by advocates.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).

% Advocate for addressing immediate, tangible harms from AI systems (e.g., bias, job displacement, surveillance). They perceive the prioritization of existential risk as diverting resources and attention from urgent present-day injustices, effectively 'paying' for this prioritization through delayed or insufficient action on their concerns.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, near_term_ai_harms_advocates, payer,
    organized, biographical, constrained, global).

% While some engage in alignment research, the focus on distant existential threats can sometimes provide cover for less scrutiny on immediate ethical implications of their deployed systems. They benefit from a narrative that shifts focus away from current accountability, but also bear some costs in terms of compliance with emerging safety standards.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, ai_developers_and_corporations, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, ai_developers_and_corporations, payer).

% Grapple with competing AI risk narratives. Those influenced by the existential risk reading prioritize funding for AGI safety research and consider regulations on advanced AI capabilities, potentially at the expense of addressing near-term societal impacts. They are constrained by political cycles and public opinion.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global research efforts and policy discussions around the singular goal of preventing an existential catastrophe from misaligned AGI, aiming to unify diverse actors under a common, urgent threat.
% TRANSFER_FUNCTION: Transfers significant financial and intellectual resources from general AI ethics and near-term harm mitigation efforts towards AGI alignment research and capability control initiatives, from present-day societal concerns to future-oriented existential prevention.
% ABSENT_VOICES: Future generations and non-existent persons are inherently absent, their interests represented by current advocates. Additionally, many communities disproportionately affected by current AI harms (e.g., marginalized groups facing algorithmic bias) are often sidelined in discussions dominated by existential risk, their concerns framed as secondary or distracting.
% DISAPPEARANCE_RATIONALE: If the prioritization of existential AI risk vanished, the global AI safety discourse would immediately reorient towards near-term harms, algorithmic justice, and responsible deployment. Funding streams would shift, research agendas would change, and policy efforts would focus on present-day regulatory frameworks rather than speculative future threats. The entire field of AI governance would undergo a significant re-prioritization.
% FOUNDING_PROBLEM: The potential for advanced artificial intelligence to develop goals misaligned with human values, leading to an uncontrollable system that could cause human extinction or irreversible disempowerment.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by a significant portion of the AI research community, prominent public intellectuals, and a growing number of policymakers, who cite theoretical arguments, scaling laws, and expert consensus on the difficulty of alignment. While some critics exist, the core concern is widely acknowledged as a legitimate, albeit distant, threat by many outside the immediate beneficiary group.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_risk_probability,
    'What is the actual probability and timescale of an extinction-level threat from misaligned AGI?',
    'Further empirical evidence on AGI capabilities, alignment techniques, and societal resilience; expert elicitation with calibrated probabilities.',
    'If the probability is significantly lower or the timescale much longer than currently assumed, the extractiveness of prioritizing this risk would increase, potentially reclassifying it towards a Snare. Conversely, higher probability or shorter timescale would reinforce its coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_risk_probability, empirical, 'Uncertainty regarding the likelihood and timing of AGI existential risk.').

omega_variable(
    resource_allocation_efficiency,
    'Are the resources currently allocated to existential AI risk research optimally deployed to mitigate the threat, or could they be more effectively used elsewhere (e.g., near-term harms)?',
    'Independent cost-benefit analysis comparing the impact of existential risk mitigation vs. near-term harm reduction, considering opportunity costs.',
    'If resources are found to be inefficiently deployed, the ''extraction'' from near-term harms advocates would be amplified, pushing the classification closer to a Snare. If highly efficient, it would strengthen the Rope aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, empirical, 'Efficiency of resource allocation for existential risk mitigation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of near-term harms advocacy structural (e.g., funding mechanisms, institutional inertia) or internalized (e.g., advocates self-censoring due to perceived lower priority)?',
    'Post-funding-shift trajectory: if near-term harms advocacy gains traction and resources after a re-prioritization, suppression was structural. If it remains marginalized despite opportunity, it suggests internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them. If purely structural, removing the external barriers would immediately empower near-term advocates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for near-term harms advocacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__existential_risk_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__existential_risk_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ai_r_tr_t2, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2, 0.16).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_prioritization__existential_risk_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(ai_r_tr_t6, ai_risk_prioritization__existential_risk_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_prioritization__existential_risk_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_prioritization__existential_risk_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ai_r_be_t2, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 4, 0.61).
narrative_ontology:measurement(ai_r_be_t6, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 6, 0.63).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(ai_r_su_t2, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2, 0.63).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 4, 0.66).
narrative_ontology:measurement(ai_r_su_t6, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 8, 0.69).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__existential_risk_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_ethics_funding_priorities).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_regulatory_frameworks).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_risk_prioritization' kernel, focusing on existential risk. Its sibling, 'ai_risk_prioritization__near_term_harms_reading', focuses on immediate societal impacts. Both are distinct constraints arising from the same underlying debate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
