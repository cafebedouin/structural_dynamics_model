% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__credentialed_expertise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__credentialed_expertise_reading, []).

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
 *   constraint_id: legitimate_knowledge_boundary__credentialed_expertise_reading
 *   human_readable: Legitimate Knowledge Derives from Credentialed Expertise
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint describes the dominant epistemic framework in which
 *   knowledge is deemed 'legitimate' only if it originates from
 *   methodologically rigorous inquiry, typically conducted by credentialed
 *   experts and validated through peer review. While this system provides a
 *   coordination function for quality control, it also operates as a powerful
 *   gatekeeping mechanism, concentrating epistemic authority and resources
 *   within established institutions and excluding alternative forms of
 *   knowledge and expertise. The claimed type is 'tangled_rope' because it
 *   genuinely solves a coordination problem (quality assurance) but does so
 *   with significant asymmetric extraction and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.78).
domain_priors:suppression_score(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.85).
domain_priors:theater_ratio(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__credentialed_expertise_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__credentialed_expertise_reading, "Legitimate Knowledge Derives from Credentialed Expertise").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__credentialed_expertise_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__credentialed_expertise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__credentialed_expertise_reading, '1a9351f1-d664-42d3-b01d-251ea4371028').
narrative_ontology:cs_kernel_codification('1a9351f1-d664-42d3-b01d-251ea4371028', formalized).
narrative_ontology:cs_authority_grounding('1a9351f1-d664-42d3-b01d-251ea4371028', expertise).
narrative_ontology:cs_interpretation_layer_present('1a9351f1-d664-42d3-b01d-251ea4371028').
narrative_ontology:cs_reading_relation('1a9351f1-d664-42d3-b01d-251ea4371028', legitimate_knowledge_boundary__experiential_pluralism_reading, forecloses).
narrative_ontology:cs_reading_relation('1a9351f1-d664-42d3-b01d-251ea4371028', legitimate_knowledge_boundary__hybrid_coproduction_reading, forecloses).
narrative_ontology:cs_axiom('1a9351f1-d664-42d3-b01d-251ea4371028', foundational, knowledge_is_objectively_discoverable).
narrative_ontology:cs_axiom_status(knowledge_is_objectively_discoverable, holdable).
narrative_ontology:cs_axiom_grounding('1a9351f1-d664-42d3-b01d-251ea4371028', knowledge_is_objectively_discoverable, empirically_contingent).
narrative_ontology:cs_axiom('1a9351f1-d664-42d3-b01d-251ea4371028', foundational, validation_requires_impartial_review).
narrative_ontology:cs_axiom_status(validation_requires_impartial_review, holdable).
narrative_ontology:cs_axiom_grounding('1a9351f1-d664-42d3-b01d-251ea4371028', validation_requires_impartial_review, conventional).
narrative_ontology:cs_reference_frame('1a9351f1-d664-42d3-b01d-251ea4371028', post_enlightenment_scientific_method).
narrative_ontology:cs_drift_state('1a9351f1-d664-42d3-b01d-251ea4371028', contemporary_post_truth_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('1a9351f1-d664-42d3-b01d-251ea4371028', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_institutions_and_publishers).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, funding_bodies).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, non_credentialed_knowledge_producers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, marginalized_communities).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, interdisciplinary_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, public_policy_makers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are individuals with advanced degrees and institutional affiliations who define, produce, and validate 'legitimate' knowledge. They benefit from the authority and resources channeled through this system, and their careers are built on its perpetuation. They set the standards for methodological rigor and peer review.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts, agenda_setter,
    institutional, biographical, arbitrage, global).

% Universities, research centers, and academic publishers provide the infrastructure for credentialing, research, and dissemination. They benefit from the prestige and financial flows associated with being arbiters of legitimate knowledge, enforcing peer review and publication standards.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_institutions_and_publishers, beneficiary,
    institutional, generational, constrained, global).

% Government agencies, foundations, and private donors allocate resources based on proposals validated by credentialed experts and institutions. They benefit from the perceived reliability and accountability of funding 'legitimate' research, reinforcing the existing epistemic hierarchy.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, funding_bodies, beneficiary,
    institutional, generational, arbitrage, national).

% Individuals or groups who generate knowledge outside formal academic structures (e.g., citizen scientists, community organizers, traditional knowledge holders). Their knowledge is often de-legitimized or ignored, and they bear the cost of exclusion from formal recognition and influence.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, non_credentialed_knowledge_producers, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, non_credentialed_knowledge_producers, excluded).

% Communities whose lived experiences and indigenous knowledge systems are not recognized as 'legitimate' by the dominant framework. They bear the cost of having their perspectives dismissed in policy and public discourse, often leading to interventions that ignore their needs or wisdom.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, marginalized_communities, payer,
    powerless, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, marginalized_communities, excluded).

% Scholars working across established disciplinary boundaries, whose methodologies or knowledge integration approaches may not fit neatly into traditional peer review categories. They face challenges in publication, funding, and recognition, bearing the cost of methodological rigidity.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, interdisciplinary_researchers, payer,
    moderate, biographical, constrained, global).

% Rely on 'expert consensus' and 'evidence-based policy' to justify decisions. They benefit from a seemingly clear, authoritative source of knowledge, but may also be constrained by its narrowness, missing crucial insights from excluded knowledge forms.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, public_policy_makers, beneficiary,
    institutional, biographical, constrained, national).

% Academics who critically analyze the social construction of scientific knowledge and expertise. They observe the operation of this constraint, documenting its mechanisms of power and exclusion, but operate largely outside its direct enforcement mechanisms.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, science_and_technology_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, high-bar standard for evaluating knowledge claims, ensuring a baseline of reliability, verifiability, and methodological rigor within formal scientific and academic systems.
% TRANSFER_FUNCTION: Transfers epistemic authority, funding, and social recognition to credentialed experts and institutions, while simultaneously de-legitimizing, marginalizing, and excluding knowledge generated outside these formal structures.
% ABSENT_VOICES: Non-credentialed knowledge producers, indigenous knowledge holders, citizen scientists, and marginalized communities whose lived experiences generate valid but non-academically validated knowledge. They would argue for broader epistemic inclusion, diverse methodologies, and recognition of alternative forms of expertise.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, the entire structure of academic authority, scientific funding, and evidence-based policy would collapse. There would be a chaotic re-evaluation of what constitutes 'truth' and 'expertise,' leading to a radical reorganization of knowledge production and dissemination.
% FOUNDING_PROBLEM: To distinguish reliable, verifiable knowledge from superstition, dogma, personal opinion, and fraud, especially as scientific inquiry became more complex and specialized, requiring rigorous methods and impartial validation.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (e.g., many credentialed experts) argue the problem is still live, citing the need for quality control against misinformation. Critics (e.g., STS scholars, social justice advocates) argue the problem is substantially solved, and the system now primarily functions as a gatekeeping mechanism, corroborated by sociological studies of science and historical analyses of epistemic exclusion.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__credentialed_expertise_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__credentialed_expertise_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legitimate_knowledge_boundary__credentialed_expertise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the substantial resources, recognition, and influence channeled exclusively to credentialed experts and institutions, often at the expense of other knowledge forms. Suppression (0.85) is severe due to the high barriers to entry (cost of education, institutional affiliation), the de-legitimization of non-credentialed knowledge, and the active enforcement of peer review and publication norms. The theater ratio (0.45) indicates that while genuine rigor exists, a significant portion of activity is performative, serving to maintain gatekeeping and reinforce existing hierarchies rather than solely advancing knowledge. Accessibility collapse is high (0.70) because alternatives are not merely ignored but actively de-legitimized within the dominant discourse. Resistance (0.60) is moderate, coming from STS scholars, social justice movements, and marginalized communities challenging epistemic injustice.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of credentialed experts, this constraint is a necessary 'rope' for quality control and scientific progress. From the perspective of excluded knowledge producers, it functions as a 'snare' that actively suppresses alternative epistemologies and perpetuates epistemic injustice. The engine's computation will reveal this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed experts, academic institutions, publishers, and funding bodies are clear beneficiaries and agenda-setters, as they directly control and profit from the system. Non-credentialed knowledge producers and marginalized communities are primary targets, bearing the costs of exclusion and de-legitimization. Interdisciplinary researchers are also targets, facing friction due to methodological rigidity. Public policy makers are beneficiaries of a seemingly reliable knowledge source but may also be constrained by its narrowness. STS scholars act as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''credentialed_expertise_reading'' of the ''legitimate_knowledge_boundary'' kernel?',
    'Comparative analysis with other readings of the same kernel, ensuring that the structural properties and stakeholder dynamics align uniquely with this specific interpretation.',
    'Misidentification would lead to incorrect classification and an inaccurate mapping of epistemic power dynamics within the broader ''legitimate_knowledge_boundary'' kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading being instantiated from the kernel.').

omega_variable(
    epistemic_function_vs_gatekeeping,
    'What proportion of the constraint''s enforcement activity genuinely serves to improve knowledge quality versus maintaining gatekeeping and institutional power?',
    'Detailed sociological studies of peer review processes, funding allocations, and publication decisions, distinguishing between quality-driven interventions and those reinforcing existing hierarchies or excluding novel approaches.',
    'If gatekeeping dominates, the constraint''s extractiveness and suppression are higher than justified by its coordination function, pushing it closer to a pure Snare. If quality improvement dominates, it leans more towards a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_function_vs_gatekeeping, empirical, 'Distinguishes genuine epistemic function from power maintenance.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (e.g., lack of funding, publication barriers) or internalized (e.g., self-censorship, belief in one''s own epistemic inferiority)?',
    'Post-exit suppression trajectory: if non-credentialed knowledge producers continue to struggle for recognition even after structural barriers are reduced, it suggests internalized suppression. Qualitative studies of epistemic self-concept among marginalized groups.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — targets carry the suppression with them after exit, making the constraint more resilient and extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for epistemic exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__credentialed_expertise_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1950, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(legi_tr_t1965, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1965, 0.25).
narrative_ontology:measurement(legi_tr_t1980, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(legi_tr_t1995, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(legi_tr_t2010, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(legi_tr_t2024, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(legi_be_t1950, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(legi_be_t1965, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1965, 0.65).
narrative_ontology:measurement(legi_be_t1980, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(legi_be_t1995, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1995, 0.74).
narrative_ontology:measurement(legi_be_t2010, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 2010, 0.76).
narrative_ontology:measurement(legi_be_t2024, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1950, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(legi_su_t1965, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1965, 0.7).
narrative_ontology:measurement(legi_su_t1980, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(legi_su_t1995, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1995, 0.8).
narrative_ontology:measurement(legi_su_t2010, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 2010, 0.83).
narrative_ontology:measurement(legi_su_t2024, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__credentialed_expertise_reading, information_standard).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary__experiential_pluralism_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, evidence_based_policy_mandate).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_publishing_model).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'legitimate_knowledge_boundary' kernel. Its structural properties and metrics are distinct from its sibling readings, 'experiential_pluralism_reading' and 'hybrid_coproduction_reading', which represent alternative framings of knowledge legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
