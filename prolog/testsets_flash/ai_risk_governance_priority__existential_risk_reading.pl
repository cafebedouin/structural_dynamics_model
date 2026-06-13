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
 *   constraint_id: ai_risk_governance_priority__existential_risk_reading
 *   human_readable: AI Risk Governance: Existential Risk Prioritization
 *   domain: AI Governance/Technology Ethics/Risk Assessment
 *
 * SUMMARY:
 *   This constraint asserts that AI risk governance must prioritize
 *   preventing superintelligence scenarios that could annihilate or
 *   permanently curtail humanity's potential. It is a specific reading of the
 *   broader 'AI risk governance priority' kernel, emphasizing long-term,
 *   high-impact, low-probability events over immediate, demonstrable harms.
 *   This prioritization shapes research agendas, funding flows, and policy
 *   discussions, often at the expense of other risk categories.
 *
 * KEY AGENTS:
 *   - x_risk_research_institutions: Primary beneficiary (institutional/arbitrage) — receive funding and legitimacy.
 *   - ai_labs_claiming_safety_leadership: Secondary beneficiary (institutional/constrained) — gain public trust and regulatory leeway.
 *   - near_term_harms_advocates: Primary victim (organized/constrained) — see their concerns deprioritized and resources diverted.
 *   - humanity_s_future_potential: Diffuse victim (universal/trapped) — the ultimate target of protection, but also potentially curtailed by misallocation of present resources.
 *   - policy_makers: Agenda setter (institutional/mobile) — allocate resources and set regulatory frameworks based on perceived priorities.
 *   - general_public: Payer/Beneficiary (moderate/constrained) — bears the costs of misallocated resources but benefits from genuine risk mitigation.
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
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "AI Risk Governance: Existential Risk Prioritization").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "AI Governance/Technology Ethics/Risk Assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, 'ce691576-4e46-42f4-b11f-996547836d11').
narrative_ontology:cs_kernel_codification('ce691576-4e46-42f4-b11f-996547836d11', distributed).
narrative_ontology:cs_authority_grounding('ce691576-4e46-42f4-b11f-996547836d11', expertise).
narrative_ontology:cs_interpretation_layer_present('ce691576-4e46-42f4-b11f-996547836d11').
narrative_ontology:cs_reading_relation('ce691576-4e46-42f4-b11f-996547836d11', ai_risk_governance_priority__near_term_harms_reading, influences).
narrative_ontology:cs_reading_relation('ce691576-4e46-42f4-b11f-996547836d11', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('ce691576-4e46-42f4-b11f-996547836d11', foundational, existential_risk_is_paramount).
narrative_ontology:cs_axiom_status(existential_risk_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('ce691576-4e46-42f4-b11f-996547836d11', existential_risk_is_paramount, deontological).
narrative_ontology:cs_axiom('ce691576-4e46-42f4-b11f-996547836d11', secondary, superintelligence_is_imminent_and_dangerous).
narrative_ontology:cs_axiom_status(superintelligence_is_imminent_and_dangerous, holdable).
narrative_ontology:cs_axiom_grounding('ce691576-4e46-42f4-b11f-996547836d11', superintelligence_is_imminent_and_dangerous, empirically_contingent).
narrative_ontology:cs_reference_frame('ce691576-4e46-42f4-b11f-996547836d11', long_term_survival_framework).
narrative_ontology:cs_drift_state('ce691576-4e46-42f4-b11f-996547836d11', contemporary_ai_development_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ce691576-4e46-42f4-b11f-996547836d11', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, near_term_harms_advocates).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, humanity_s_future_potential).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__existential_risk_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_risk_governance_priority__existential_risk_reading, 'none', 1).

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
 *   The extractiveness (0.65) is high because resources and attention are significantly diverted towards highly speculative, long-term risks, potentially at the expense of addressing demonstrable present harms. Suppression (0.4) is moderate; while alternative views are not strictly forbidden, they are often marginalized in dominant discourse and funding. Theater ratio (0.55) is high because a significant portion of 'safety' work under this framing is performative or directed at highly theoretical problems, rather than practical, verifiable interventions for current systems. Accessibility collapse (0.7) is high because once this prioritization is accepted, alternative risk frameworks become difficult to implement or fund. Resistance (0.3) is moderate, as there are active, organized groups advocating for different priorities.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of x-risk research institutions, this constraint is a necessary, even existential, coordination mechanism. From the perspective of near-term harms advocates, it is an extractive mechanism that diverts resources and attention from urgent, demonstrable problems. Policy makers navigate these competing claims, often influenced by the most vocal or well-funded narratives.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk research institutions and AI labs claiming safety leadership are beneficiaries, as this prioritization directs funding and legitimacy towards their work. Near-term harms advocates are victims, as their concerns are deprioritized. Humanity's future potential is a diffuse victim, as the prioritization might misallocate resources in the present, potentially curtailing future options. Policy makers are agenda setters, shaping the discourse and resource allocation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it genuinely attempts to coordinate efforts against a perceived existential threat (a coordination function), but it also exhibits asymmetric extraction by diverting resources and attention towards specific, often speculative, research agendas that benefit certain institutions, while potentially neglecting other, more immediate harms (asymmetric extraction). The 'mandate' to prevent existential risk is live, but its 'function' in practice may be captured by specific interests, leading to a form of mandatrophy where the original broad goal is narrowed to serve specific beneficiaries. The high theater ratio suggests that some 'safety' work is more about maintaining the narrative of prioritization than effective, broad-spectrum risk mitigation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine prioritization of existential risk, or a rhetorical framing to secure resources for specific research agendas?',
    'Analysis of resource allocation patterns: if funding disproportionately flows to speculative, long-term alignment research over demonstrable near-term safety, it supports the latter.',
    'If a rhetorical framing, the constraint''s effective extractiveness is higher, as it diverts resources from more immediate, verifiable harms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''existential_risk_reading'' of the ''ai_risk_governance_priority'' kernel. Sibling readings (''near_term_harms_reading'', ''bridge_reading'') would shift the victim set and resource allocation. The disagreement is located in the definition of ''priority'' and the scope of ''risk''.').

omega_variable(
    speculative_vs_demonstrated_harms,
    'To what extent are the ''superintelligence scenarios'' a demonstrable threat versus a speculative one, and how does this affect the legitimacy of prioritization?',
    'Empirical progress in AI capabilities and independent expert consensus on the feasibility and timeline of AGI and superintelligence.',
    'If the threat remains highly speculative, the prioritization appears more extractive, diverting resources from demonstrable harms. If the threat becomes more concrete, the prioritization gains legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(speculative_vs_demonstrated_harms, empirical, 'The balance between speculative future risks and demonstrated present harms is a core tension in this reading.').

omega_variable(
    mandatrophy_of_focus,
    'Does the focus on existential risk, while potentially valid, lead to mandatrophy regarding present, demonstrable AI harms?',
    'Tracking the resolution of near-term harms (e.g., algorithmic bias, labor displacement) in parallel with existential risk mitigation efforts. If near-term harms worsen or are neglected, it indicates mandatrophy.',
    'If mandatrophy is present, the constraint functions as a Snare for those affected by near-term harms, as resources and attention are diverted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_focus, empirical, 'Analysis of whether the existential risk focus inadvertently neglects or exacerbates present AI harms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 5, 0.45).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(ai_r_tr_t15, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 15, 0.55).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(ai_r_be_t15, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(ai_r_su_t15, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 15, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__existential_risk_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ai_risk_governance_priority' kernel. This 'existential_risk_reading' prioritizes long-term, catastrophic risks. The 'near_term_harms_reading' prioritizes immediate, demonstrable harms. The 'bridge_reading' attempts to integrate both. Each is a distinct constraint with different beneficiaries, victims, and extractiveness profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
