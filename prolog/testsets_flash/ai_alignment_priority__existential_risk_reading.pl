% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_alignment_priority__existential_risk_reading
 *   human_readable: AI Alignment: Existential Risk Priority
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint defines AI alignment as primarily focused on preventing
 *   catastrophic loss of control over advanced AI systems, prioritizing
 *   existential safety above all other concerns. It is one reading of the
 *   broader 'AI Alignment Priority' kernel. This framing directs significant
 *   resources and attention towards highly speculative future risks, often at
 *   the expense of addressing present, demonstrable harms from AI. The
 *   constraint is actively enforced through funding mechanisms, academic
 *   discourse, and public advocacy that marginalize alternative framings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, 0.85).
domain_priors:suppression_score(ai_alignment_priority__existential_risk_reading, 0.7).
domain_priors:theater_ratio(ai_alignment_priority__existential_risk_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__existential_risk_reading, "AI Alignment: Existential Risk Priority").
narrative_ontology:topic_domain(ai_alignment_priority__existential_risk_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__existential_risk_reading, 'e90f7005-f88f-486c-a5c9-74d3028596af').
narrative_ontology:cs_kernel_codification('e90f7005-f88f-486c-a5c9-74d3028596af', distributed).
narrative_ontology:cs_authority_grounding('e90f7005-f88f-486c-a5c9-74d3028596af', expertise).
narrative_ontology:cs_interpretation_layer_present('e90f7005-f88f-486c-a5c9-74d3028596af').
narrative_ontology:cs_reading_relation('e90f7005-f88f-486c-a5c9-74d3028596af', ai_alignment_priority__nearterm_harms_reading, influences).
narrative_ontology:cs_reading_relation('e90f7005-f88f-486c-a5c9-74d3028596af', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('e90f7005-f88f-486c-a5c9-74d3028596af', foundational, existential_risk_is_paramount).
narrative_ontology:cs_axiom_status(existential_risk_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('e90f7005-f88f-486c-a5c9-74d3028596af', existential_risk_is_paramount, deontological).
narrative_ontology:cs_axiom('e90f7005-f88f-486c-a5c9-74d3028596af', secondary, ai_capabilities_outpace_safety).
narrative_ontology:cs_axiom_status(ai_capabilities_outpace_safety, holdable).
narrative_ontology:cs_axiom_grounding('e90f7005-f88f-486c-a5c9-74d3028596af', ai_capabilities_outpace_safety, empirically_contingent).
narrative_ontology:cs_reference_frame('e90f7005-f88f-486c-a5c9-74d3028596af', precautionary_principle_for_ai).
narrative_ontology:cs_drift_state('e90f7005-f88f-486c-a5c9-74d3028596af', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e90f7005-f88f-486c-a5c9-74d3028596af', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__existential_risk_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, long_term_future_humanity).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, existential_risk_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, nearterm_ai_harm_victims).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, ai_safety_field_resources).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Focus on preventing catastrophic loss of control over advanced AI, prioritizing scenarios that could lead to human extinction. They advocate for significant resource allocation to this specific problem, often through adversarial red-teaming methodologies. Their professional identity is deeply tied to this framing of AI safety.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, existential_risk_researchers, agenda_setter,
    institutional, civilizational, identity_locked, global).

% The ultimate beneficiary of preventing existential risks from AI. This entity is abstract and cannot act, but its hypothetical welfare is the primary justification for the constraint. Its interests are represented by the existential risk researchers.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, long_term_future_humanity, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_alignment_priority__existential_risk_reading, long_term_future_humanity).

% Individuals and communities currently experiencing harms from deployed AI systems (e.g., algorithmic bias, surveillance, job displacement). Their concerns are often deprioritized or framed as secondary to existential risks, leading to a lack of resources and attention for their immediate suffering.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, nearterm_ai_harm_victims, payer,
    powerless, immediate, trapped, local).

% The overall pool of funding, talent, and institutional attention within the AI safety and ethics domain. This constraint directs a disproportionate share of these resources towards existential risk research, potentially at the expense of other critical safety and ethical concerns.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, ai_safety_field_resources, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_non_agent(ai_alignment_priority__existential_risk_reading, ai_safety_field_resources).

% Advocate for a balanced approach to AI alignment, addressing both near-term harms and long-term existential risks as complementary. They find their proposals for integrated resource allocation and research agendas often sidelined by the dominant existential risk framing.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, integrated_ai_safety_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the global AI safety research agenda and resource allocation towards a singular, long-term goal of preventing human extinction from advanced AI, providing a clear, unifying priority for a diverse field.
% TRANSFER_FUNCTION: Transfers significant intellectual and financial resources from addressing near-term, tangible harms of AI to speculative, future-oriented existential risks. It also transfers agency over AI safety priorities to a specific group of researchers.
% ABSENT_VOICES: The direct victims of near-term AI harms (e.g., those affected by biased algorithms, job displacement) are largely absent from the high-level discussions and resource allocation decisions, as their concerns are deemed secondary to existential threats. Advocates for integrated or near-term approaches are often marginalized.
% DISAPPEARANCE_RATIONALE: If this priority vanished, the AI safety field would immediately re-evaluate its resource allocation, likely shifting significant funding and research focus towards more immediate, demonstrable harms and integrated approaches. The political and research landscape of AI governance would fundamentally reorganize.
% FOUNDING_PROBLEM: The perceived threat of advanced AI systems developing uncontrollable capabilities that could lead to human extinction, a problem deemed unprecedented and catastrophic.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by a significant portion of the AI research community, prominent public figures, and dedicated research institutions. While the severity and imminence are contested by some, the existence of the problem itself is widely acknowledged as a plausible, if speculative, future risk. Corroboration comes from within the AI research community and a segment of the broader scientific community, though not universally from outside the directly benefiting parties.
narrative_ontology:disappearance_verdict(ai_alignment_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__existential_risk_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_alignment_priority__existential_risk_reading, 'none', 1).

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
 *   The extractiveness is high (0.85) because it diverts substantial resources and attention from other pressing AI safety issues, creating a 'cost' for those whose concerns are deprioritized. Suppression (0.7) is also high, as alternative framings are actively marginalized in funding, publication, and public discourse. The theater ratio (0.4) reflects that while genuine safety work occurs, a portion of the activity serves to maintain the priority framing itself, rather than directly addressing the full spectrum of AI risks. The metrics show a trend of increasing extractiveness and suppression over time, indicating a hardening of this specific priority.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of existential risk researchers, this constraint is a vital 'rope' coordinating humanity's efforts against an ultimate threat. From the perspective of near-term harm victims or integrated safety advocates, it functions as a 'snare' or 'tangled rope,' extracting resources and attention from their immediate suffering under the guise of a higher, more abstract good. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Existential risk researchers and the abstract 'long-term future humanity' are the primary beneficiaries, as the constraint channels resources to their specific concerns. Near-term AI harm victims and the broader 'AI safety field resources' are the victims, as their needs are deprioritized. Integrated safety advocates are excluded, as their attempts to balance priorities are resisted. The directionality for existential risk researchers is near 0.0 (full beneficiary), while for near-term harm victims it is near 1.0 (full target).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing existential risk) is framed as perpetually live, preventing a mandatrophy resolution. However, the increasing extractiveness and suppression suggest that the 'coordination' function is increasingly serving to maintain a specific research agenda and resource flow, rather than solely addressing the problem it was founded to solve. If the problem were truly 'dead' (i.e., existential risk was definitively mitigated), the constraint would likely persist due to institutional inertia and the beneficiaries' entrenched positions, becoming a piton. The 'contested' status of the founding problem prevents a clear mandatrophy declaration, but the observed drift points towards a potential future mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_risk_probability_calibration,
    'What is the actual, empirically grounded probability of AI-induced existential catastrophe within a given timeframe, and how does this calibrate the urgency and resource allocation?',
    'Development of robust, falsifiable methodologies for forecasting AI capabilities and failure modes, coupled with independent expert elicitation and meta-analysis.',
    'A significantly lower probability would undermine the justification for extreme resource diversion, potentially reclassifying the constraint towards a piton or a less extractive tangled rope. A higher, well-corroborated probability would strengthen its claim as a necessary rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_risk_probability_calibration, empirical, 'Uncertainty regarding the actual likelihood and imminence of AI existential risk.').

omega_variable(
    resource_allocation_efficiency,
    'Is the current allocation of AI safety resources, heavily skewed towards existential risk, the most effective way to mitigate the full spectrum of AI risks (including near-term harms)?',
    'Comprehensive, independent cost-benefit analysis comparing the impact of existential risk research funding versus funding for near-term harm mitigation and integrated approaches.',
    'If current allocation is found inefficient, it would strengthen the argument for reclassifying towards a snare or a more extractive tangled rope, highlighting the misdirection of resources. If efficient, it would support the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, empirical, 'Efficiency of resource allocation given the full scope of AI risks.').

omega_variable(
    framing_under_determination,
    'Is the ''existential risk priority'' framing a necessary and accurate representation of AI safety, or is it a conceptual choice that marginalizes other valid concerns?',
    'Analysis of the historical and sociological development of the AI safety field, examining how different framings gained or lost prominence and their impact on research agendas and funding.',
    'If it''s primarily a conceptual choice, it highlights the constructed nature of the constraint and the power dynamics involved in maintaining it, potentially shifting classification towards a more extractive type. If it''s demonstrably the most accurate framing, it reinforces its coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination, conceptual, 'The ''existential risk priority'' is a conceptual framing that marginalizes other valid concerns.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__existential_risk_reading, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t2015, ai_alignment_priority__existential_risk_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(ai_a_tr_t2018, ai_alignment_priority__existential_risk_reading, theater_ratio, 2018, 0.25).
narrative_ontology:measurement(ai_a_tr_t2021, ai_alignment_priority__existential_risk_reading, theater_ratio, 2021, 0.3).
narrative_ontology:measurement(ai_a_tr_t2024, ai_alignment_priority__existential_risk_reading, theater_ratio, 2024, 0.4).
narrative_ontology:measurement(ai_a_tr_t2027, ai_alignment_priority__existential_risk_reading, theater_ratio, 2027, 0.45).
narrative_ontology:measurement_basis(ai_a_tr_t2027, projected).
narrative_ontology:measurement(ai_a_tr_t2030, ai_alignment_priority__existential_risk_reading, theater_ratio, 2030, 0.5).
narrative_ontology:measurement_basis(ai_a_tr_t2030, projected).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t2015, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(ai_a_be_t2018, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2018, 0.7).
narrative_ontology:measurement(ai_a_be_t2021, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2021, 0.78).
narrative_ontology:measurement(ai_a_be_t2024, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2024, 0.85).
narrative_ontology:measurement(ai_a_be_t2027, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2027, 0.88).
narrative_ontology:measurement_basis(ai_a_be_t2027, projected).
narrative_ontology:measurement(ai_a_be_t2030, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2030, 0.9).
narrative_ontology:measurement_basis(ai_a_be_t2030, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t2015, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(ai_a_su_t2018, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2018, 0.5).
narrative_ontology:measurement(ai_a_su_t2021, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2021, 0.6).
narrative_ontology:measurement(ai_a_su_t2024, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2024, 0.7).
narrative_ontology:measurement(ai_a_su_t2027, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2027, 0.75).
narrative_ontology:measurement_basis(ai_a_su_t2027, projected).
narrative_ontology:measurement(ai_a_su_t2030, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2030, 0.8).
narrative_ontology:measurement_basis(ai_a_su_t2030, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__existential_risk_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__nearterm_harms_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__integrated_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_ethics_research_funding).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'AI Alignment Priority' kernel. This 'existential risk' reading structurally influences the 'near-term harms' and 'integrated' readings by diverting resources and attention, making their implementation more difficult.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
