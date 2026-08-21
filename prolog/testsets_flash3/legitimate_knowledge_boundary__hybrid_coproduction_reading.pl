% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__hybrid_coproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__hybrid_coproduction_reading, []).

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
 *   constraint_id: legitimate_knowledge_boundary__hybrid_coproduction_reading
 *   human_readable: Hybrid Co-production Standard for Legitimate Knowledge
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint defines legitimate knowledge as requiring the integration
 *   of methodological rigor and experiential validity through co-production
 *   processes. It is a reading of the 'legitimate_knowledge_boundary' kernel,
 *   emphasizing a hybrid approach. While aiming for inclusivity, it creates
 *   new barriers and costs for those who must adapt to its dual validation
 *   requirements, particularly for communities lacking co-production
 *   capacity. The constraint is claimed as a Rope by its proponents, but its
 *   operational metrics suggest a Tangled Rope due to the active enforcement
 *   and asymmetric costs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.45).
domain_priors:suppression_score(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.3).
domain_priors:theater_ratio(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__hybrid_coproduction_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__hybrid_coproduction_reading, "Hybrid Co-production Standard for Legitimate Knowledge").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__hybrid_coproduction_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__hybrid_coproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__hybrid_coproduction_reading, '446b30de-44e4-428b-ac7a-4cc6448cd212').
narrative_ontology:cs_kernel_codification('446b30de-44e4-428b-ac7a-4cc6448cd212', formalized).
narrative_ontology:cs_authority_grounding('446b30de-44e4-428b-ac7a-4cc6448cd212', practice).
narrative_ontology:cs_interpretation_layer_present('446b30de-44e4-428b-ac7a-4cc6448cd212').
narrative_ontology:cs_reading_relation('446b30de-44e4-428b-ac7a-4cc6448cd212', legitimate_knowledge_boundary__credentialed_expertise_reading, influences).
narrative_ontology:cs_reading_relation('446b30de-44e4-428b-ac7a-4cc6448cd212', legitimate_knowledge_boundary__experiential_pluralism_reading, influences).
narrative_ontology:cs_axiom('446b30de-44e4-428b-ac7a-4cc6448cd212', foundational, integrated_knowledge_is_superior).
narrative_ontology:cs_axiom_status(integrated_knowledge_is_superior, holdable).
narrative_ontology:cs_axiom_grounding('446b30de-44e4-428b-ac7a-4cc6448cd212', integrated_knowledge_is_superior, instrumental).
narrative_ontology:cs_axiom('446b30de-44e4-428b-ac7a-4cc6448cd212', foundational, dual_validation_is_necessary).
narrative_ontology:cs_axiom_status(dual_validation_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('446b30de-44e4-428b-ac7a-4cc6448cd212', dual_validation_is_necessary, conventional).
narrative_ontology:cs_reference_frame('446b30de-44e4-428b-ac7a-4cc6448cd212', integrated_epistemic_pluralism).
narrative_ontology:cs_drift_state('446b30de-44e4-428b-ac7a-4cc6448cd212', contemporary_funding_landscape, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('446b30de-44e4-428b-ac7a-4cc6448cd212', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_facilitators).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, interdisciplinary_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, traditional_academic_disciplines).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, marginalized_communities_lacking_coproduction_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations and individuals specializing in designing and managing co-production processes. They benefit from the demand for their expertise and the legitimacy conferred by this standard. They actively promote and enforce the dual validation requirements.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_facilitators, agenda_setter,
    organized, biographical, mobile, regional).

% Academics whose work naturally bridges methodological rigor and experiential knowledge. This standard legitimizes their approach and opens new funding avenues, though it also imposes additional coordination costs.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, interdisciplinary_researchers, beneficiary,
    moderate, biographical, constrained, national).

% Established academic fields that must adapt their practices to include experiential validity and co-production, incurring costs in training, re-evaluating metrics, and restructuring research. They resist the perceived dilution of 'rigor'.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, traditional_academic_disciplines, payer,
    institutional, generational, constrained, global).

% Communities whose experiential knowledge is valued but who lack the resources, time, or institutional support to engage effectively in complex co-production processes, leading to their knowledge being undervalued or appropriated despite the standard's intent.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, marginalized_communities_lacking_coproduction_capacity, payer,
    powerless, generational, trapped, local).

% Scholars and institutions who prioritize traditional methodological rigor and peer review, viewing co-production as a compromise of scientific standards. They are often sidelined in discussions about 'legitimate knowledge' under this reading.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_expertise_advocates, excluded,
    powerful, generational, constrained, global).

% Activists and community leaders who champion experiential knowledge but find the 'methodological rigor' requirement of co-production to be an unnecessary barrier or a tool for re-centering academic power. They would prefer a less constrained validation process.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_pluralism_advocates, excluded,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse knowledge forms (scientific, indigenous, local, lived) to address complex problems that require both empirical evidence and contextual understanding, preventing epistemic silos and fostering more relevant solutions.
% TRANSFER_FUNCTION: Transfers legitimacy and resources to knowledge claims that successfully integrate methodological rigor and experiential validity, while transferring costs (time, effort, capacity building) to those who must adapt to this dual validation standard.
% ABSENT_VOICES: Advocates for pure credentialed expertise or pure experiential pluralism are often marginalized in the co-production discourse, as their positions are seen as undermining the 'hybrid' ideal. They would argue for simpler, less mediated validation paths.
% DISAPPEARANCE_RATIONALE: If this standard vanished, the landscape of knowledge production would fragment. Funding for interdisciplinary, community-engaged research would diminish, and the integration of diverse knowledge forms would become ad-hoc or cease, leading to less relevant and less trusted knowledge in complex domains.
% FOUNDING_PROBLEM: Traditional knowledge systems (academic, scientific) were seen as detached from societal needs and often failed to address problems relevant to marginalized communities, while purely experiential knowledge struggled for broader recognition and impact.
% FOUNDING_PROBLEM_CORROBORATION: Many intergovernmental bodies (e.g., IPCC, WHO), funding agencies, and community-based research networks attest to the ongoing need for integrated knowledge approaches, citing persistent gaps between scientific findings and actionable, equitable solutions. This corroboration comes from outside the direct beneficiaries of co-production facilitation.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__hybrid_coproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__hybrid_coproduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).
:- end_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) arises from the significant investment required to meet both methodological and experiential standards, and the costs of facilitating co-production, which often fall disproportionately on less resourced actors. Suppression (0.30) is present because knowledge claims failing to meet this dual standard are actively de-legitimized or excluded from influential platforms. Theater ratio (0.20) is moderate; while genuine co-production occurs, some processes may be performative, designed to signal inclusivity without deep integration. The metrics show a gradual increase in extractiveness and suppression as the standard becomes more institutionalized and its enforcement mechanisms mature.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this hybrid standard (co-production facilitators, interdisciplinary researchers) perceive it as a necessary and equitable coordination mechanism (a Rope). However, those who bear the costs of adaptation or exclusion (traditional academics, marginalized communities) experience it as an extractive structure (a Snare or Tangled Rope) that imposes new forms of gatekeeping. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Co-production facilitators and interdisciplinary researchers are beneficiaries, as the standard creates demand for their skills and legitimizes their work. Traditional academic disciplines and marginalized communities lacking co-production capacity are payers, bearing the costs of adaptation or exclusion. Advocates for pure expertise or pure experientialism are excluded, as their positions are not fully accommodated by this hybrid standard.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coproduction_capacity_asymmetry,
    'Does the requirement for co-production inadvertently create new barriers for marginalized communities who lack the resources or institutional capacity to engage in such processes?',
    'Empirical studies tracking participation rates, resource allocation, and perceived burden on different community groups in co-production initiatives. Analysis of funding mechanisms for capacity building.',
    'If capacity asymmetry is significant and unmitigated, the constraint''s effective extractiveness and suppression for marginalized communities are higher than measured, potentially reclassifying their seat as Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coproduction_capacity_asymmetry, empirical, 'Whether the co-production requirement disproportionately burdens certain communities.').

omega_variable(
    rigor_experiential_integration_fidelity,
    'To what extent is the ''integration'' of methodological rigor and experiential validity genuinely achieved, versus one being subordinated to the other in practice?',
    'Qualitative and quantitative analysis of co-produced knowledge outputs, assessing the balance of influence from both knowledge forms, and the extent to which each informs the other''s validation criteria.',
    'If integration is consistently superficial or one form dominates, the constraint''s claimed coordination function is undermined, increasing its theater_ratio and potentially shifting its classification towards Snare for the subordinated knowledge form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rigor_experiential_integration_fidelity, empirical, 'Fidelity of integration between methodological rigor and experiential validity.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the legitimacy of co-produced knowledge primarily derived from its integrated nature, or from the institutional power of those who champion and fund co-production?',
    'Comparative analysis of knowledge uptake and policy influence for co-produced knowledge versus other forms, controlling for institutional backing. Examination of funding trends and power dynamics within co-production networks.',
    'If institutional power is the primary driver of legitimacy, the constraint''s coordination function is weaker, and its extractiveness is higher, as it primarily serves to channel resources and recognition through specific institutional actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Whether legitimacy stems from integration or institutional power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(legi_tr_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement(legi_tr_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(legi_tr_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(legi_tr_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(legi_be_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(legi_be_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(legi_be_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(legi_be_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(legi_su_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 5, 0.24).
narrative_ontology:measurement(legi_su_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 10, 0.27).
narrative_ontology:measurement(legi_su_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 15, 0.29).
narrative_ontology:measurement(legi_su_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__hybrid_coproduction_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary__credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary__experiential_pluralism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'legitimate_knowledge_boundary' kernel. This 'hybrid_coproduction_reading' emphasizes the integration of methodological rigor and experiential validity, influencing but not foreclosing the 'credentialed_expertise_reading' and 'experiential_pluralism_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
