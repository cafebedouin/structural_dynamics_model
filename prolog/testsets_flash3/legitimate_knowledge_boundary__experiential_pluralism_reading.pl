% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__experiential_pluralism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__experiential_pluralism_reading, []).

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
 *   constraint_id: legitimate_knowledge_boundary__experiential_pluralism_reading
 *   human_readable: Legitimate Knowledge Boundary: Experiential Pluralism Reading
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'experiential pluralism' reading of the
 *   legitimate knowledge boundary, where lived experience and community
 *   validation are central to knowledge legitimacy, and methodological
 *   standards are considered one tool among many. It aims to democratize
 *   knowledge production and challenge traditional epistemic hierarchies. The
 *   metrics reflect a relatively low-extraction, low-suppression constraint,
 *   as it primarily functions by opening access and validating previously
 *   excluded forms of knowledge, rather than coercively extracting from
 *   existing systems.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.25).
domain_priors:suppression_score(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.15).
domain_priors:theater_ratio(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__experiential_pluralism_reading, rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__experiential_pluralism_reading, "Legitimate Knowledge Boundary: Experiential Pluralism Reading").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__experiential_pluralism_reading, "epistemology/science_and_technology_studies/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__experiential_pluralism_reading, 'a97c0c03-c4e8-42e7-9d63-7f94b1413d9a').
narrative_ontology:cs_kernel_codification('a97c0c03-c4e8-42e7-9d63-7f94b1413d9a', distributed).
narrative_ontology:cs_authority_grounding('a97c0c03-c4e8-42e7-9d63-7f94b1413d9a', practice).
narrative_ontology:cs_interpretation_layer_present('a97c0c03-c4e8-42e7-9d63-7f94b1413d9a').
narrative_ontology:cs_reading_relation('a97c0c03-c4e8-42e7-9d63-7f94b1413d9a', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('a97c0c03-c4e8-42e7-9d63-7f94b1413d9a', legitimate_knowledge_boundary__hybrid_coproduction_reading, coexists_with).
narrative_ontology:cs_axiom('a97c0c03-c4e8-42e7-9d63-7f94b1413d9a', foundational, lived_experience_as_primary_epistemic_source).
narrative_ontology:cs_axiom_status(lived_experience_as_primary_epistemic_source, holdable).
narrative_ontology:cs_axiom_grounding('a97c0c03-c4e8-42e7-9d63-7f94b1413d9a', lived_experience_as_primary_epistemic_source, deontological).
narrative_ontology:cs_axiom('a97c0c03-c4e8-42e7-9d63-7f94b1413d9a', foundational, community_validation_as_legitimacy_criterion).
narrative_ontology:cs_axiom_status(community_validation_as_legitimacy_criterion, holdable).
narrative_ontology:cs_axiom_grounding('a97c0c03-c4e8-42e7-9d63-7f94b1413d9a', community_validation_as_legitimacy_criterion, conventional).
narrative_ontology:cs_reference_frame('a97c0c03-c4e8-42e7-9d63-7f94b1413d9a', decolonized_epistemic_pluralism).
narrative_ontology:cs_drift_state('a97c0c03-c4e8-42e7-9d63-7f94b1413d9a', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('a97c0c03-c4e8-42e7-9d63-7f94b1413d9a', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, activist_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, traditional_academic_institutions).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, standpoint_epistemology).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, participatory_action_research).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their lived experiences are recognized as a primary source of legitimate knowledge, empowering them to define research agendas and validate findings relevant to their contexts. This reading reduces the epistemic gatekeeping they often face.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_communities, beneficiary,
    organized, generational, mobile, local).

% Their methodologies, which prioritize community engagement and experiential knowledge, are validated and gain broader acceptance. They benefit from a more inclusive definition of expertise that aligns with their practice.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, activist_researchers, beneficiary,
    moderate, biographical, mobile, regional).

% Are challenged to broaden their definitions of legitimate knowledge and expertise, potentially requiring shifts in tenure criteria, funding priorities, and pedagogical approaches. They bear the cost of adapting to new epistemic standards.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, traditional_academic_institutions, payer,
    institutional, generational, constrained, national).

% Their exclusive claim to knowledge authority is diluted, as methodological rigor becomes 'one tool among many' rather than the sole arbiter. They may experience a loss of status or influence if they do not adapt to incorporate experiential validation.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts, payer,
    powerful, biographical, constrained, global).

% Must navigate competing claims of knowledge legitimacy when making decisions, potentially integrating insights from both experiential and methodological sources. This reading complicates their traditional reliance on 'expert consensus'.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, policy_makers, observer,
    institutional, immediate, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse knowledge producers by establishing a common framework where different forms of knowledge (experiential, methodological) can be recognized and integrated, fostering broader participation in knowledge creation.
% TRANSFER_FUNCTION: Transfers epistemic authority and validation power from traditionally credentialed experts to individuals and communities with lived experience, and to processes of community validation.
% ABSENT_VOICES: Those who exclusively uphold a positivist, method-driven epistemology are often excluded from the core conversation, as their framework is seen as overly restrictive and potentially colonialist. They would argue for universal methodological standards.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the epistemic landscape would revert to a more hierarchical structure, with credentialed expertise regaining its dominant position. Marginalized communities would lose a key framework for asserting their knowledge claims, and participatory research methods would be devalued.
% FOUNDING_PROBLEM: Traditional knowledge systems systematically excluded and devalued the insights of marginalized communities and those without formal credentials, leading to incomplete or biased understandings of complex social problems.
% FOUNDING_PROBLEM_CORROBORATION: Sociologists of science, critical theorists, and indigenous scholars widely corroborate the historical and ongoing problem of epistemic injustice, citing numerous examples of marginalized knowledge being dismissed or appropriated. This corroboration comes from outside the direct beneficiaries of this reading.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__experiential_pluralism_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__experiential_pluralism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimate_knowledge_boundary__experiential_pluralism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).
:- end_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because this reading primarily expands the definition of legitimate knowledge, rather than imposing heavy costs on existing knowledge producers. Suppression is low (0.15) as its persistence relies on advocacy and adoption, not active coercion against alternative readings. Resistance is high (0.70) because it actively challenges established norms and institutions. Accessibility collapse is low (0.30) as it aims to open, not close, pathways to knowledge validation. Theater ratio is low (0.10) as its proponents are genuinely committed to its principles.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized communities, this reading is a liberating force, a pure rope that opens access to knowledge production. From the perspective of traditional credentialed experts, it may be perceived as a threat to rigor or a 'tangled rope' that dilutes established standards, even though its direct extraction from them is low. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized communities and activist researchers are clear beneficiaries, as their knowledge claims gain legitimacy and influence. Traditional academic institutions and credentialed experts are payers, as they face pressure to adapt and share epistemic authority. Policy makers are observers, navigating the implications of this broadened knowledge base.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is relatively new and actively contested, so mandatrophy is not a primary concern. Its mandate is live: to address ongoing epistemic injustices. The classification as 'rope' reflects its function in coordinating a more inclusive knowledge ecosystem, rather than extracting from it. It avoids mislabeling by focusing on its generative, rather than extractive, impact on knowledge production.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_application,
    'To what extent can ''experiential pluralism'' be universally applied across all domains of knowledge (e.g., natural sciences vs. social sciences)?',
    'Empirical case studies demonstrating successful application in diverse domains, or identification of inherent limitations in certain fields.',
    'If universally applicable, this reading''s influence would expand significantly, challenging the dominance of other readings. If limited, its scope would be constrained, allowing other readings to retain authority in specific domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_application, empirical, 'Ambiguity regarding the universal applicability of experiential pluralism.').

omega_variable(
    quality_control_mechanism,
    'How does ''community validation'' effectively guard against misinformation or biased knowledge claims, particularly when communities are insular or lack diverse perspectives?',
    'Development and implementation of robust, transparent community validation protocols that incorporate mechanisms for critical self-reflection and engagement with diverse viewpoints.',
    'If effective quality control mechanisms are demonstrated, the legitimacy of this reading is strengthened. If not, it risks being dismissed as susceptible to bias or misinformation, bolstering the arguments of the ''credentialed_expertise_reading''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_control_mechanism, conceptual, 'Uncertainty about the robustness of community validation as a quality control mechanism.').

omega_variable(
    epistemic_injustice_resolution,
    'Does this reading genuinely resolve epistemic injustices, or does it risk creating new forms of exclusion or marginalization by devaluing other forms of knowledge?',
    'Longitudinal studies tracking the impact of this reading on diverse knowledge producers, assessing whether it fosters genuine inclusion or merely shifts the locus of power.',
    'If it demonstrably reduces epistemic injustice without creating new forms, its normative force is amplified. If it creates new exclusions, its ethical justification is weakened, potentially leading to a re-evaluation of its ''beneficiary'' status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_injustice_resolution, preference, 'Whether the reading truly resolves epistemic injustices or creates new ones.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__experiential_pluralism_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(legi_tr_t5, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(legi_tr_t10, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(legi_tr_t15, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(legi_tr_t20, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(legi_be_t5, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(legi_be_t10, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 10, 0.23).
narrative_ontology:measurement(legi_be_t15, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 15, 0.24).
narrative_ontology:measurement(legi_be_t20, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(legi_su_t5, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(legi_su_t10, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 10, 0.13).
narrative_ontology:measurement(legi_su_t15, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 15, 0.14).
narrative_ontology:measurement(legi_su_t20, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__experiential_pluralism_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, hybrid_coproduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'legitimate_knowledge_boundary' kernel. It directly influences the 'credentialed_expertise_reading' by challenging its foundational assumptions and the 'hybrid_coproduction_reading' by providing a distinct alternative to its integrative approach.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
