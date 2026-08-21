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
 *   constraint_id: legitimate_knowledge_boundary__credentialed_expertise_reading
 *   human_readable: Legitimate Knowledge Boundary: Credentialed Expertise Reading
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'credentialed expertise' reading of the
 *   legitimate knowledge boundary, asserting that only knowledge produced
 *   through methodologically rigorous inquiry and validated by credentialed
 *   peer review is legitimate. It operates with high barriers to entry,
 *   centralized gatekeeping, and asymmetric enforcement of methodological
 *   rigor, leading to significant extraction from non-credentialed knowledge
 *   producers. The claimed type is 'tangled_rope' because it purports to
 *   offer a coordination function (quality control) while simultaneously
 *   extracting through its exclusionary mechanisms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.7).
domain_priors:suppression_score(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.8).
domain_priors:theater_ratio(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__credentialed_expertise_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__credentialed_expertise_reading, "Legitimate Knowledge Boundary: Credentialed Expertise Reading").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__credentialed_expertise_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__credentialed_expertise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__credentialed_expertise_reading, 'c474a19b-7e6e-41dc-b89f-dc4a87106050').
narrative_ontology:cs_kernel_codification('c474a19b-7e6e-41dc-b89f-dc4a87106050', formalized).
narrative_ontology:cs_authority_grounding('c474a19b-7e6e-41dc-b89f-dc4a87106050', expertise).
narrative_ontology:cs_interpretation_layer_present('c474a19b-7e6e-41dc-b89f-dc4a87106050').
narrative_ontology:cs_reading_relation('c474a19b-7e6e-41dc-b89f-dc4a87106050', legitimate_knowledge_boundary__experiential_pluralism_reading, forecloses).
narrative_ontology:cs_reading_relation('c474a19b-7e6e-41dc-b89f-dc4a87106050', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('c474a19b-7e6e-41dc-b89f-dc4a87106050', foundational, knowledge_is_objective_and_universal).
narrative_ontology:cs_axiom_status(knowledge_is_objective_and_universal, holdable).
narrative_ontology:cs_axiom_grounding('c474a19b-7e6e-41dc-b89f-dc4a87106050', knowledge_is_objective_and_universal, empirically_contingent).
narrative_ontology:cs_axiom('c474a19b-7e6e-41dc-b89f-dc4a87106050', foundational, validation_requires_impartial_peer_review).
narrative_ontology:cs_axiom_status(validation_requires_impartial_peer_review, holdable).
narrative_ontology:cs_axiom_grounding('c474a19b-7e6e-41dc-b89f-dc4a87106050', validation_requires_impartial_peer_review, conventional).
narrative_ontology:cs_reference_frame('c474a19b-7e6e-41dc-b89f-dc4a87106050', enlightenment_epistemic_ideal).
narrative_ontology:cs_drift_state('c474a19b-7e6e-41dc-b89f-dc4a87106050', contemporary_post_truth_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c474a19b-7e6e-41dc-b89f-dc4a87106050', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_institutions).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, funding_bodies).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, non_credentialed_knowledge_producers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, marginalized_communities).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, public_seeking_diverse_knowledge).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are individuals with formal academic qualifications and positions who define, produce, and validate 'legitimate' knowledge through peer review and publication. They benefit from epistemic authority, funding, and social recognition, and actively enforce methodological standards.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts, beneficiary).

% Universities, research centers, and professional societies that house credentialed experts, administer peer review, and grant credentials. They benefit from prestige, funding, and control over knowledge production, enforcing the boundary through hiring, promotion, and publication policies.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_institutions, beneficiary).

% Government agencies, foundations, and private corporations that allocate resources for research. They benefit from the perceived legitimacy and reliability of credentialed knowledge, using it to justify policy decisions or product development. They reinforce the constraint by prioritizing funding for credentialed researchers and institutions.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, funding_bodies, beneficiary,
    institutional, generational, arbitrage, national).

% Individuals or groups (e.g., citizen scientists, independent researchers, activists) who produce knowledge outside formal academic or credentialed channels. Their knowledge is often dismissed, de-legitimized, or ignored, making it difficult to gain recognition or influence policy.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, non_credentialed_knowledge_producers, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, non_credentialed_knowledge_producers, excluded).

% Communities whose traditional, indigenous, or lived-experience-based knowledge systems are not recognized as 'legitimate' by the dominant credentialed framework. They bear the cost of epistemic injustice, as their insights are excluded from policy and public discourse, often leading to harmful interventions.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, marginalized_communities, payer,
    powerless, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, marginalized_communities, excluded).

% The broader public that relies on knowledge for decision-making but is limited to sources validated by credentialed expertise. They pay the cost of a narrower epistemic landscape, potentially missing valuable insights from non-credentialed sources, and may experience distrust when expert consensus conflicts with their lived realities.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, public_seeking_diverse_knowledge, payer,
    moderate, biographical, constrained, global).

% Academics who critically analyze the social construction of scientific knowledge, the role of expertise, and power dynamics within epistemic systems. They observe and document the constraint's operation without directly benefiting or paying its costs, offering an external analytical perspective.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, science_and_technology_studies_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__credentialed_expertise_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared standard for evaluating knowledge claims, aiming to ensure reliability, reproducibility, and cumulative progress within scientific and academic disciplines, thereby reducing epistemic chaos and promoting trust in specific forms of knowledge.
% TRANSFER_FUNCTION: Transfers epistemic authority, social capital, and financial resources to credentialed experts and institutions, while simultaneously de-legitimizing, marginalizing, and excluding knowledge produced by non-credentialed individuals or groups.
% ABSENT_VOICES: Indigenous knowledge holders, citizen scientists, community-based researchers, and those whose lived experience generates valid but non-credentialed insights. They would advocate for epistemic pluralism, alternative validation methods, and a more inclusive definition of 'legitimate' knowledge.
% DISAPPEARANCE_RATIONALE: If the exclusive claim of credentialed expertise to legitimate knowledge vanished overnight, there would be a profound reordering of epistemic hierarchies. Diverse knowledge systems would gain prominence, funding flows would diversify, and public trust in knowledge would fragment or re-align around new, more localized validation mechanisms. The current structure of academic institutions and scientific authority would be fundamentally challenged.
% FOUNDING_PROBLEM: The historical need to distinguish reliable, verifiable knowledge from superstition, dogma, and individual bias, particularly as scientific inquiry became more complex and specialized, requiring rigorous methods and peer scrutiny to build collective understanding.
% FOUNDING_PROBLEM_CORROBORATION: While the original problem of ensuring reliable knowledge is widely acknowledged, independent historians of science, philosophers of science, and STS scholars corroborate that the *exclusive* reliance on credentialed expertise has, over time, shifted from purely epistemic hygiene to a mechanism for maintaining power and privilege within specific knowledge-producing communities. This corroboration comes from outside the direct beneficiaries of the system.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__credentialed_expertise_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__credentialed_expertise_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legitimate_knowledge_boundary__credentialed_expertise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.7) reflects the significant costs borne by those outside the credentialed system, including de-legitimization and lack of access to resources and influence. Suppression (0.8) is high due to the active gatekeeping mechanisms of peer review, credentialing bodies, and funding allocations that actively exclude alternative knowledge forms. The theater ratio (0.4) indicates that while genuine methodological rigor is present, a substantial portion of the system's activity is performative, serving to maintain the exclusive boundary rather than solely ensuring quality. The increasing trends in extractiveness and suppression over the interval reflect the hardening of these boundaries and the increasing professionalization and institutionalization of knowledge production.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of credentialed experts, the system is a necessary 'rope' for quality control and scientific progress. From the perspective of non-credentialed knowledge producers and marginalized communities, it functions as a 'snare' or 'tangled_rope,' actively excluding their valid insights and maintaining an extractive hierarchy. The engine's computation of per-seat classifications will highlight this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed experts, academic institutions, and funding bodies are clear beneficiaries and agenda-setters, as they control the definition, production, and dissemination of 'legitimate' knowledge, accruing authority and resources. Non-credentialed knowledge producers and marginalized communities are primary targets, bearing the costs of exclusion and epistemic injustice. The public, while benefiting from some vetted knowledge, also pays the cost of a restricted epistemic landscape. STS scholars act as analytical observers, documenting these dynamics.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_gatekeeping_vs_quality_control,
    'To what extent does the credentialed expertise system primarily function as a genuine quality control mechanism versus a mechanism for epistemic gatekeeping and power consolidation?',
    'Comparative studies of knowledge outcomes (e.g., accuracy, utility, social impact) from credentialed vs. non-credentialed systems, particularly in domains where both operate. Analysis of funding and publication biases.',
    'If primarily gatekeeping, the constraint''s extractiveness and suppression are higher than currently measured, and its coordination function is largely theatrical. If primarily quality control, the measured extraction is a necessary cost of coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemic_gatekeeping_vs_quality_control, empirical, 'Distinguishing genuine quality control from power-driven exclusion.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of non-credentialed knowledge primarily structural (lack of access to credentialing, publication, funding) or internalized (non-credentialed individuals doubting their own knowledge''s legitimacy)?',
    'Post-intervention studies: if providing access to resources and platforms for non-credentialed knowledge producers leads to a rapid increase in recognized knowledge, suppression is largely structural. If self-doubt and deference persist, internalized suppression is significant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them even if external barriers are lowered. This would make the constraint more resilient to external challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for non-credentialed knowledge.').

omega_variable(
    universality_of_rigor_claim,
    'Is the concept of ''methodological rigor'' as enforced by credentialed expertise universally applicable across all knowledge domains and cultural contexts, or is it a culturally and historically specific construct?',
    'Philosophical and anthropological analysis of diverse knowledge systems, examining whether alternative forms of validation (e.g., consensus, spiritual insight, practical efficacy) achieve comparable ''rigor'' within their own frameworks.',
    'If rigor is culturally specific, the claim of universal legitimacy for credentialed knowledge is conceptually flawed, weakening the constraint''s epistemic grounding and supporting the validity of alternative knowledge systems. This would shift the constraint''s classification towards a more constructed, less ''natural'' form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_of_rigor_claim, conceptual, 'The conceptual validity of universal methodological rigor.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__credentialed_expertise_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1980, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(legi_tr_t1988, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1988, 0.25).
narrative_ontology:measurement(legi_tr_t1996, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1996, 0.3).
narrative_ontology:measurement(legi_tr_t2004, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 2004, 0.35).
narrative_ontology:measurement(legi_tr_t2012, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 2012, 0.38).
narrative_ontology:measurement(legi_tr_t2020, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(legi_be_t1980, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(legi_be_t1988, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1988, 0.56).
narrative_ontology:measurement(legi_be_t1996, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1996, 0.62).
narrative_ontology:measurement(legi_be_t2004, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 2004, 0.66).
narrative_ontology:measurement(legi_be_t2012, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 2012, 0.68).
narrative_ontology:measurement(legi_be_t2020, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 2020, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1980, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(legi_su_t1988, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1988, 0.68).
narrative_ontology:measurement(legi_su_t1996, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1996, 0.74).
narrative_ontology:measurement(legi_su_t2004, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 2004, 0.77).
narrative_ontology:measurement(legi_su_t2012, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 2012, 0.79).
narrative_ontology:measurement(legi_su_t2020, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 2020, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__credentialed_expertise_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
