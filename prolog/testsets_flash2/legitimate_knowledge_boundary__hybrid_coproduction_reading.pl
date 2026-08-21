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
 *   This constraint describes the 'hybrid co-production' reading of
 *   legitimate knowledge, which asserts that valid knowledge requires the
 *   integration of both methodological rigor (from scientific traditions) and
 *   experiential validity (from lived experience), achieved through
 *   co-production processes. This reading seeks to overcome the limitations
 *   of purely expert-driven or purely experience-driven approaches. It is
 *   presented as a 'rope' due to its genuine coordination function, but its
 *   extractiveness and suppression reflect the costs of institutionalizing
 *   and enforcing this dual validation, particularly on traditional academic
 *   structures and pure experiential advocates.
 *
 * KEY AGENTS:
 *   - coproduction_facilitators: Agenda-setter (organized/constrained) — promotes and implements co-production.
 *   - marginalized_communities: Beneficiary (powerless/constrained) — gains epistemic recognition.
 *   - interdisciplinary_researchers: Beneficiary (moderate/mobile) — finds legitimacy for their work.
 *   - traditional_academic_institutions: Payer (institutional/constrained) — bears costs of adaptation.
 *   - pure_experiential_advocates: Payer (moderate/constrained) — resists methodological imposition.
 *   - credentialed_experts: Excluded (powerful/mobile) — views co-production as diluting rigor.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.45).
domain_priors:suppression_score(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.3).
domain_priors:theater_ratio(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__hybrid_coproduction_reading, rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__hybrid_coproduction_reading, "Hybrid Co-production Standard for Legitimate Knowledge").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__hybrid_coproduction_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__hybrid_coproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'a593774e-7c86-4cc7-b84c-16f7e31a0654').
narrative_ontology:cs_kernel_codification('a593774e-7c86-4cc7-b84c-16f7e31a0654', formalized).
narrative_ontology:cs_authority_grounding('a593774e-7c86-4cc7-b84c-16f7e31a0654', practice).
narrative_ontology:cs_interpretation_layer_present('a593774e-7c86-4cc7-b84c-16f7e31a0654').
narrative_ontology:cs_reading_relation('a593774e-7c86-4cc7-b84c-16f7e31a0654', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('a593774e-7c86-4cc7-b84c-16f7e31a0654', legitimate_knowledge_boundary__experiential_pluralism_reading, coexists_with).
narrative_ontology:cs_axiom('a593774e-7c86-4cc7-b84c-16f7e31a0654', foundational, epistemic_justice_requires_integration).
narrative_ontology:cs_axiom_status(epistemic_justice_requires_integration, holdable).
narrative_ontology:cs_axiom_grounding('a593774e-7c86-4cc7-b84c-16f7e31a0654', epistemic_justice_requires_integration, deontological).
narrative_ontology:cs_axiom('a593774e-7c86-4cc7-b84c-16f7e31a0654', foundational, robust_knowledge_from_diverse_sources).
narrative_ontology:cs_axiom_status(robust_knowledge_from_diverse_sources, holdable).
narrative_ontology:cs_axiom_grounding('a593774e-7c86-4cc7-b84c-16f7e31a0654', robust_knowledge_from_diverse_sources, empirically_contingent).
narrative_ontology:cs_reference_frame('a593774e-7c86-4cc7-b84c-16f7e31a0654', integrated_knowledge_ecosystem).
narrative_ontology:cs_drift_state('a593774e-7c86-4cc7-b84c-16f7e31a0654', contemporary_epistemic_contestation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a593774e-7c86-4cc7-b84c-16f7e31a0654', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_facilitators).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, interdisciplinary_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, traditional_academic_institutions).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, pure_experiential_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations and individuals dedicated to designing and implementing co-production processes. They benefit from the legitimacy this standard confers on their work and actively promote its adoption, but face challenges in securing consistent funding and institutional buy-in.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_facilitators, agenda_setter,
    organized, biographical, constrained, regional).

% Communities whose knowledge and experiences are often excluded by traditional scientific methods. They benefit from having their experiential validity recognized and integrated into research, leading to more relevant and equitable outcomes, but still face power imbalances in co-production settings.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, marginalized_communities, beneficiary,
    powerless, generational, constrained, local).

% Academics and practitioners who bridge disciplinary divides and are open to integrating diverse forms of knowledge. This standard legitimizes their approach, offering new avenues for funding and impact, though it requires adapting to new methodologies and collaborative practices.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, interdisciplinary_researchers, beneficiary,
    moderate, biographical, mobile, national).

% Universities and research bodies structured around conventional peer-review and disciplinary silos. They bear the cost of adapting to new co-production requirements, which can challenge established hierarchies, funding models, and tenure criteria. Resistance often comes from inertia and resource allocation challenges.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, traditional_academic_institutions, payer,
    institutional, generational, constrained, national).

% Groups who prioritize lived experience as the sole or primary arbiter of knowledge validity. They find the requirement for methodological rigor to be an imposition that dilutes the authenticity of experiential knowledge, viewing it as a concession to dominant scientific paradigms.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, pure_experiential_advocates, payer,
    moderate, biographical, constrained, local).

% Scientists and scholars whose authority is primarily derived from formal credentials and adherence to established disciplinary methods. They may view co-production as diluting scientific rigor or introducing bias, preferring traditional peer-review mechanisms for validation.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_experts, excluded,
    powerful, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the integration of diverse knowledge forms (methodological and experiential) to produce more robust, relevant, and legitimate knowledge outcomes, particularly for complex societal challenges.
% TRANSFER_FUNCTION: Transfers epistemic authority and resources from purely credentialed expertise to hybrid co-production processes, distributing the benefits of knowledge creation more broadly while imposing new requirements on all participants.
% ABSENT_VOICES: Pure credentialed experts, who would argue for the primacy of traditional scientific rigor, and radical experiential pluralists, who would reject any imposition of methodological standards, are both marginalized by this hybrid approach.
% DISAPPEARANCE_RATIONALE: If this standard vanished, the momentum for integrating diverse knowledge forms would dissipate, leading to a re-entrenchment of traditional academic silos and a loss of legitimacy for knowledge claims that rely on co-production. Research funding and impact pathways would shift significantly.
% FOUNDING_PROBLEM: Traditional knowledge production often failed to address complex societal problems effectively because it excluded experiential insights and lacked legitimacy among affected communities, leading to irrelevant or distrusted findings.
% FOUNDING_PROBLEM_CORROBORATION: Co-production practitioners and marginalized communities widely attest that the problem of epistemic injustice and irrelevance remains live. Some traditional academic institutions acknowledge the need for greater societal relevance, providing corroboration from outside the primary beneficiaries.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__hybrid_coproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__hybrid_coproduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) reflects the significant investment required to establish and maintain co-production infrastructure, as well as the 'cost' to traditional institutions in terms of adapting their practices and to experiential advocates in accepting methodological constraints. Suppression (0.30) is present because this standard actively pushes back against both purely expert-driven and purely experiential approaches, requiring adherence to its dual validation criteria. The theater ratio is low (0.10) because the co-production processes are generally genuine, though some performative adoption exists. The increasing trend in extractiveness and suppression over time reflects the growing institutionalization and enforcement of this standard, which inevitably creates more friction and costs for those who must adapt.
 *
 * PERSPECTIVAL GAP:
 *   Co-production facilitators and marginalized communities experience this as a beneficial rope, opening new pathways for legitimate knowledge. Traditional academic institutions and pure experiential advocates, however, experience it as a tangled rope or even a snare, as it imposes new requirements and challenges their established ways of knowing or validating knowledge. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Co-production facilitators, marginalized communities, and interdisciplinary researchers are beneficiaries, as the constraint legitimizes their work and empowers their voices. Traditional academic institutions and pure experiential advocates are payers, as they must adapt to new standards or compromise their preferred epistemic frameworks. Credentialed experts are excluded, as their preferred mode of knowledge production is not fully recognized as legitimate under this hybrid standard.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling genuine efforts at epistemic justice as pure extraction. While there are costs and resistance, the core function of integrating diverse knowledge forms for more legitimate outcomes remains active. The rising extractiveness is a signal to monitor for potential drift towards a tangled rope if the costs of co-production become disproportionate to its benefits or if the 'integration' becomes a one-way imposition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    co_production_authenticity,
    'To what extent are ''co-production processes'' genuinely collaborative and equitable, versus being performative inclusions that maintain existing power hierarchies?',
    'Longitudinal ethnographic studies of co-production projects, assessing shifts in power dynamics, resource allocation, and epistemic authority over time, as perceived by all participants.',
    'If largely performative, the constraint''s effective extractiveness and suppression are higher than measured, as it masks continued epistemic injustice. This would shift its classification towards a Tangled Rope or Snare for marginalized communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_production_authenticity, empirical, 'Assessing the genuine equity and power-sharing in co-production processes.').

omega_variable(
    methodological_experiential_balance,
    'Is the ''integration'' truly balanced, or does methodological rigor still implicitly dominate, requiring experiential knowledge to conform to scientific frameworks?',
    'Content analysis of co-produced outputs and process evaluations, specifically tracking how conflicts between methodological and experiential claims are resolved, and whose epistemic standards ultimately prevail.',
    'If methodological rigor consistently dominates, the constraint''s suppression of pure experiential knowledge is higher, and its claimed coordination function is weaker, pushing it towards a Tangled Rope for pure experiential advocates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodological_experiential_balance, conceptual, 'Balance between methodological and experiential demands in co-production.').

omega_variable(
    institutional_resistance_threshold,
    'At what point does the cost of adapting to co-production become prohibitive for traditional academic institutions, leading to active subversion rather than reluctant compliance?',
    'Comparative case studies of institutional change, tracking resource allocation, policy shifts, and faculty resistance in response to co-production mandates across different universities.',
    'If the cost becomes prohibitive, traditional academic institutions would shift from ''payer'' to ''active resistor'', potentially leading to a breakdown of the constraint or its reclassification as a Snare if enforcement becomes overtly coercive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_resistance_threshold, empirical, 'Threshold for institutional resistance to co-production adaptation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(legi_tr_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement(legi_tr_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(legi_tr_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(legi_tr_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(legi_be_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(legi_be_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(legi_be_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(legi_su_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 5, 0.23).
narrative_ontology:measurement(legi_su_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 10, 0.26).
narrative_ontology:measurement(legi_su_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 15, 0.28).
narrative_ontology:measurement(legi_su_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__hybrid_coproduction_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_pluralism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'legitimate_knowledge_boundary' kernel. It represents the hybrid co-production approach, which seeks to integrate methodological rigor and experiential validity. It influences and coexists with the 'credentialed_expertise_reading' and 'experiential_pluralism_reading' by offering an alternative framework for knowledge legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
