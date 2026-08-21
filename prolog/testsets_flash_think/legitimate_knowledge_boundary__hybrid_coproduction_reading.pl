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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: legitimate_knowledge_boundary__hybrid_coproduction_reading
 *   human_readable: Legitimate Knowledge Boundary: Hybrid Co-production Reading
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint defines legitimate knowledge as requiring the integration
 *   of methodological rigor and experiential validity through co-production
 *   processes. It is a specific reading of the broader
 *   'legitimate_knowledge_boundary' kernel, emphasizing a dual validation
 *   standard and active engagement between different knowledge holders. The
 *   constraint aims to solve problems of knowledge legitimacy and
 *   applicability but imposes significant costs and enforces specific modes
 *   of interaction, leading to its classification as a Tangled Rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.6).
domain_priors:suppression_score(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.7).
domain_priors:theater_ratio(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__hybrid_coproduction_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__hybrid_coproduction_reading, "Legitimate Knowledge Boundary: Hybrid Co-production Reading").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__hybrid_coproduction_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__hybrid_coproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__hybrid_coproduction_reading, '521378d2-3597-4b3a-8999-3aef0ec769d7').
narrative_ontology:cs_kernel_codification('521378d2-3597-4b3a-8999-3aef0ec769d7', formalized).
narrative_ontology:cs_authority_grounding('521378d2-3597-4b3a-8999-3aef0ec769d7', practice).
narrative_ontology:cs_interpretation_layer_present('521378d2-3597-4b3a-8999-3aef0ec769d7').
narrative_ontology:cs_reading_relation('521378d2-3597-4b3a-8999-3aef0ec769d7', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('521378d2-3597-4b3a-8999-3aef0ec769d7', legitimate_knowledge_boundary__experiential_pluralism_reading, coexists_with).
narrative_ontology:cs_axiom('521378d2-3597-4b3a-8999-3aef0ec769d7', foundational, knowledge_is_co_produced).
narrative_ontology:cs_axiom_status(knowledge_is_co_produced, holdable).
narrative_ontology:cs_axiom_grounding('521378d2-3597-4b3a-8999-3aef0ec769d7', knowledge_is_co_produced, conventional).
narrative_ontology:cs_axiom('521378d2-3597-4b3a-8999-3aef0ec769d7', foundational, dual_validation_necessary).
narrative_ontology:cs_axiom_status(dual_validation_necessary, holdable).
narrative_ontology:cs_axiom_grounding('521378d2-3597-4b3a-8999-3aef0ec769d7', dual_validation_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('521378d2-3597-4b3a-8999-3aef0ec769d7', integrated_epistemic_framework).
narrative_ontology:cs_drift_state('521378d2-3597-4b3a-8999-3aef0ec769d7', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('521378d2-3597-4b3a-8999-3aef0ec769d7', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_facilitators).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, integrated_knowledge_users).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, pure_credentialed_experts).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, unvalidated_experiential_knowers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, implement, and manage co-production processes, benefiting from the institutionalization of these methods and the resources allocated to them. They enforce the dual standards of methodological rigor and experiential validity.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_facilitators, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from knowledge outputs that are more robust, legitimate, and applicable to complex societal problems due to the integration of diverse perspectives and methods. They seek out and utilize such knowledge.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, integrated_knowledge_users, beneficiary,
    moderate, biographical, mobile, global).

% Are required to adapt their research practices, engage with non-expert communities, and share epistemic authority. This involves significant investment in new skills and processes, and a potential loss of exclusive control over knowledge production.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, pure_credentialed_experts, payer,
    powerful, biographical, constrained, global).

% Are required to articulate their experiential knowledge in ways that can be integrated with methodological rigor, often needing to engage with formal processes or language that may be unfamiliar or disempowering. Their knowledge is not accepted as legitimate without this integration.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, unvalidated_experiential_knowers, payer,
    powerless, biographical, constrained, local).

% Advocate for a model where legitimate knowledge primarily derives from methodologically rigorous inquiry validated by credentialed peer review, viewing co-production as diluting scientific standards or introducing bias. They are often marginalized in co-production discourse.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_expertise_advocates, excluded,
    institutional, generational, analytical, global).

% Advocate for a model where legitimate knowledge arises primarily from lived experience and community validation, viewing methodological standards as potentially oppressive or irrelevant. They are often marginalized in co-production discourse when rigor is heavily emphasized.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_pluralism_advocates, excluded,
    institutional, generational, analytical, global).

% Analyze the dynamics of knowledge production, legitimacy, and power, including the promises and pitfalls of co-production processes. They provide critical insights into how this constraint operates in practice.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, science_and_technology_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_facilitators).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__hybrid_coproduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates diverse forms of knowledge (methodological rigor and experiential validity) to produce more robust, legitimate, and actionable insights for complex societal challenges that neither siloed expertise nor unvalidated experience can fully address.
% TRANSFER_FUNCTION: Transfers epistemic authority, resources, and the burden of integration towards co-production processes and away from purely siloed or unintegrated knowledge production. It also transfers the cost of dual validation to both expert and experiential knowers.
% ABSENT_VOICES: Advocates for purely credentialed expertise or purely experiential pluralism are often excluded, as their positions challenge the fundamental premise of mandatory integration. They would argue that the co-production process is either unnecessary, burdensome, or compromises the integrity of their preferred knowledge form.
% DISAPPEARANCE_RATIONALE: If the requirement for hybrid co-production vanished, knowledge production would likely revert to siloed approaches, leading to a resurgence of legitimacy crises for scientific knowledge and a failure to effectively address complex, transdisciplinary problems that demand integrated insights.
% FOUNDING_PROBLEM: Knowledge produced by siloed expertise often lacks societal legitimacy or practical applicability, while purely experiential knowledge may lack methodological rigor, leading to fragmented and mistrusted insights for complex societal challenges.
% FOUNDING_PROBLEM_CORROBORATION: Scholars in Science and Technology Studies (STS), public policy experts, and community organizers consistently attest to the ongoing challenges of knowledge legitimacy, applicability, and public trust, supporting the continued need for integrated approaches. This corroboration comes from outside the direct beneficiaries of co-production infrastructure.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__hybrid_coproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__hybrid_coproduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.60) reflects the substantial investment in time, resources, and epistemic authority required for genuine co-production and dual validation. Suppression (0.70) is high because knowledge claims failing to meet both methodological and experiential standards, or those produced outside co-production, are deemed illegitimate. The theater ratio (0.20) is relatively low, acknowledging that while performative co-production exists, the core intent of this reading is genuine integration. The claimed type is Tangled Rope because it serves a genuine coordination function (integrating knowledge) but does so through active enforcement and asymmetric extraction of costs and authority from those who must adapt to its demands.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of co-production facilitators and integrated knowledge users, this constraint is a necessary and beneficial coordination mechanism. However, from the perspective of pure credentialed experts and unvalidated experiential knowers, it imposes significant burdens and extracts concessions, potentially feeling like an enforced compromise rather than a pure benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Co-production facilitators are beneficiaries and agenda-setters, gaining institutional legitimacy and resources. Integrated knowledge users are beneficiaries of more robust knowledge. Pure credentialed experts and unvalidated experiential knowers are payers, as they bear the costs of adapting their practices and sharing authority. Advocates for alternative readings are excluded, as their positions are structurally incompatible with the constraint's core premise.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    co_production_genuineness_ambiguity,
    'Is the ''integration'' in co-production genuinely balanced, or does it, in practice, privilege methodological rigor over experiential validity (or vice versa)?',
    'Empirical studies of co-production outcomes, analyzing power dynamics, resource allocation, and epistemic influence within specific projects. Longitudinal tracking of whose knowledge claims are ultimately validated and how.',
    'If one form of knowledge is consistently privileged, the constraint''s effective extractiveness and suppression would be higher for the marginalized group, potentially reclassifying it closer to a Snare for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_production_genuineness_ambiguity, empirical, 'Whether co-production achieves genuine epistemic parity or maintains existing hierarchies.').

omega_variable(
    co_production_cost_distribution,
    'Are the costs and burdens of co-production (time, effort, adaptation) equitably distributed among all participants, or disproportionately borne by specific groups?',
    'Detailed ethnographic studies and participant surveys within co-production projects, quantifying time commitments, emotional labor, and perceived benefits/costs for different stakeholder groups.',
    'If costs are disproportionately borne by one group (e.g., unvalidated experiential knowers), the constraint''s effective extractiveness for that group would be higher than currently estimated, indicating a more extractive dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_production_cost_distribution, empirical, 'Equity of burden sharing in co-production processes.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the ''legitimate_knowledge_boundary'' kernel, or is it a variant of one of the sibling readings?',
    'Conceptual analysis of the core axioms and their practical implications, comparing them against the ''credentialed_expertise_reading'' and ''experiential_pluralism_reading'' to identify irreducible differences in epistemic grounding and enforcement mechanisms.',
    'If it were found to be a variant, it would be merged with the dominant sibling, simplifying the kernel''s structure but potentially obscuring the unique challenges of integration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint as a distinct reading within the legitimate knowledge boundary kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(legi_tr_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(legi_tr_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(legi_tr_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(legi_tr_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(legi_be_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(legi_be_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(legi_be_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(legi_be_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(legi_su_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(legi_su_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(legi_su_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(legi_su_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__hybrid_coproduction_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
