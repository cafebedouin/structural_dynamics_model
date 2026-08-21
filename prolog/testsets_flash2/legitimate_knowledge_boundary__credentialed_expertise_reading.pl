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
 *   constraint_id: legitimate_knowledge_boundary__credentialed_expertise_reading
 *   human_readable: Legitimate Knowledge Boundary: Credentialed Expertise Reading
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint defines legitimate knowledge as that which emerges from
 *   methodologically rigorous inquiry, validated by credentialed peer review.
 *   It is one reading of the broader 'legitimate_knowledge_boundary' kernel.
 *   This reading emphasizes formal academic processes, creating high barriers
 *   to entry for non-credentialed knowledge producers and centralizing
 *   epistemic authority within established institutions. The metrics reflect
 *   a system that, while providing coordination, also extracts significantly
 *   from those outside its formal structures and actively suppresses
 *   alternative epistemic claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.65).
domain_priors:suppression_score(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.78).
domain_priors:theater_ratio(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__credentialed_expertise_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__credentialed_expertise_reading, "Legitimate Knowledge Boundary: Credentialed Expertise Reading").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__credentialed_expertise_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__credentialed_expertise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__credentialed_expertise_reading, '700835fd-d2d3-4804-a46a-51f44b4107a3').
narrative_ontology:cs_kernel_codification('700835fd-d2d3-4804-a46a-51f44b4107a3', formalized).
narrative_ontology:cs_authority_grounding('700835fd-d2d3-4804-a46a-51f44b4107a3', lineage).
narrative_ontology:cs_interpretation_layer_present('700835fd-d2d3-4804-a46a-51f44b4107a3').
narrative_ontology:cs_reading_relation('700835fd-d2d3-4804-a46a-51f44b4107a3', legitimate_knowledge_boundary__experiential_pluralism_reading, coexists_with).
narrative_ontology:cs_reading_relation('700835fd-d2d3-4804-a46a-51f44b4107a3', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('700835fd-d2d3-4804-a46a-51f44b4107a3', foundational, epistemic_validity_requires_formal_validation).
narrative_ontology:cs_axiom_status(epistemic_validity_requires_formal_validation, holdable).
narrative_ontology:cs_axiom_grounding('700835fd-d2d3-4804-a46a-51f44b4107a3', epistemic_validity_requires_formal_validation, conventional).
narrative_ontology:cs_axiom('700835fd-d2d3-4804-a46a-51f44b4107a3', foundational, credentialing_ensures_competence).
narrative_ontology:cs_axiom_status(credentialing_ensures_competence, holdable).
narrative_ontology:cs_axiom_grounding('700835fd-d2d3-4804-a46a-51f44b4107a3', credentialing_ensures_competence, empirically_contingent).
narrative_ontology:cs_reference_frame('700835fd-d2d3-4804-a46a-51f44b4107a3', enlightenment_scientific_ideal).
narrative_ontology:cs_drift_state('700835fd-d2d3-4804-a46a-51f44b4107a3', contemporary_sts_critiques, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('700835fd-d2d3-4804-a46a-51f44b4107a3', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_institutions).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, funding_bodies).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, non_credentialed_knowledge_producers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, marginalized_communities).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, interdisciplinary_researchers).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, scientific_method_supremacy).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, epistemic_authority_of_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are individuals with advanced degrees and institutional affiliations who define methodological rigor, conduct peer review, and largely control access to publication and funding. They benefit from the system's prestige and resource allocation.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts, agenda_setter,
    institutional, generational, constrained, global).

% Universities and research centers whose legitimacy and funding depend on housing credentialed experts and producing 'legitimate' knowledge. They benefit from the system's gatekeeping function, which reinforces their central role.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_institutions, beneficiary,
    institutional, generational, constrained, national).

% Government agencies and private foundations that allocate research grants based on peer-reviewed proposals. They benefit from a clear, if narrow, definition of legitimate knowledge, which simplifies their decision-making and legitimizes their allocations.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, funding_bodies, beneficiary,
    institutional, biographical, mobile, national).

% Individuals or groups (e.g., citizen scientists, indigenous knowledge holders, community organizers) who produce valuable knowledge but lack formal credentials or institutional backing. Their knowledge is often dismissed or devalued, forcing them to seek alternative validation or remain unheard.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, non_credentialed_knowledge_producers, payer,
    powerless, biographical, identity_locked, local).

% Communities whose lived experiences generate critical insights, but whose epistemic frameworks are often excluded from 'legitimate' discourse. They bear the cost of having their knowledge ignored or appropriated without recognition, leading to policy decisions that do not reflect their realities.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, marginalized_communities, payer,
    powerless, generational, trapped, local).

% Researchers who attempt to bridge disciplinary boundaries or integrate diverse methodologies often face challenges in peer review, as their work may not fit neatly into established 'rigorous' frameworks. They pay a cost in slower career progression and difficulty securing funding.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, interdisciplinary_researchers, payer,
    moderate, biographical, constrained, global).

% Rely on 'legitimate' knowledge to inform policy decisions, often prioritizing peer-reviewed scientific consensus. They observe the debates but are structurally incentivized to defer to credentialed expertise, even when it conflicts with other forms of knowledge.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, public_policy_makers, observer,
    institutional, immediate, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a standardized process for validating knowledge claims, ensuring a baseline of methodological quality and creating a shared epistemic currency for scientific and academic discourse.
% TRANSFER_FUNCTION: Transfers epistemic authority, prestige, and resources (funding, publication access) from non-credentialed or non-peer-reviewed knowledge producers to credentialed experts and academic institutions.
% ABSENT_VOICES: Knowledge producers from indigenous communities, citizen science movements, and grassroots organizations are often excluded from the formal peer-review process; they would argue for broader epistemic inclusion and recognition of diverse knowledge forms.
% DISAPPEARANCE_RATIONALE: If the credentialed peer-review system vanished overnight, the landscape of recognized knowledge would fragment. New, diverse validation mechanisms would emerge, but also a proliferation of unvetted claims. Funding and academic prestige would need new allocation criteria, fundamentally reorganizing how society identifies and trusts knowledge.
% FOUNDING_PROBLEM: To establish a reliable, verifiable method for distinguishing sound knowledge from speculation, dogma, or error, particularly in the wake of the Enlightenment and the rise of modern science.
% FOUNDING_PROBLEM_CORROBORATION: Credentialed experts and academic institutions attest the problem is still live, citing the need to combat misinformation and maintain scientific integrity. Critics from marginalized communities and STS scholars acknowledge the historical problem but argue the current system has become overly exclusive and self-serving, rather than purely problem-solving.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__credentialed_expertise_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__credentialed_expertise_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimate_knowledge_boundary__credentialed_expertise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because the system concentrates epistemic authority and resources, devaluing or ignoring knowledge produced outside its framework. Suppression is very high (0.78) due to the active gatekeeping of peer review, publication, and funding, which effectively excludes alternative knowledge forms. Theater ratio is moderate (0.20); while genuine methodological rigor is applied, a portion of the 'rigor' serves to maintain the authority of the gatekeepers rather than purely advancing knowledge. Accessibility collapse is high (0.70) because once this definition of legitimacy is accepted, alternatives are largely dismissed. Resistance is moderate (0.45) from those whose knowledge is excluded, but this resistance is often fragmented and lacks institutional power.
 *
 * PERSPECTIVAL GAP:
 *   Credentialed experts perceive this as a necessary 'rope' for quality control and coordination, ensuring reliable knowledge. Non-credentialed producers and marginalized communities experience it as a 'snare' that systematically excludes their valid insights, serving to maintain an elite epistemic hierarchy. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed experts, academic institutions, and funding bodies are clear beneficiaries, as the system channels prestige, resources, and authority to them. Non-credentialed knowledge producers, marginalized communities, and interdisciplinary researchers are victims, bearing the cost of exclusion and devaluation. Public policy makers are observers, often deferring to the system's output.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_exclusion_justification,
    'Is the exclusion of non-credentialed knowledge producers a necessary function of quality control, or a mechanism for maintaining epistemic power structures?',
    'Empirical studies comparing the reliability and utility of knowledge produced by credentialed vs. non-credentialed sources in specific contexts, particularly in areas where marginalized communities hold unique expertise.',
    'If primarily a power mechanism, the extractiveness and suppression metrics would be re-evaluated upwards, potentially reclassifying the constraint as a Snare. If genuinely necessary for quality, the coordination function would be emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_exclusion_justification, empirical, 'Distinguishing between genuine quality control and power-based exclusion in epistemic gatekeeping.').

omega_variable(
    methodological_rigor_universality,
    'Is ''methodological rigor'' a universal, objective standard, or is it culturally and disciplinarily contingent, serving to privilege certain forms of inquiry?',
    'Comparative analysis of epistemic practices across diverse cultures and knowledge traditions, examining how ''rigor'' is defined and applied in different contexts.',
    'If culturally contingent, the claim of universal legitimacy for credentialed expertise weakens, supporting the ''experiential_pluralism_reading'' and increasing the perceived extractiveness of this constraint from diverse knowledge systems.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(methodological_rigor_universality, conceptual, 'Assessing the universality vs. contingency of methodological rigor as a standard for legitimate knowledge.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (lack of access to publication, funding) or internalized (non-credentialed producers self-censor or doubt their own knowledge claims)?',
    'Post-exit suppression trajectory: if knowledge claims from non-credentialed sources gain traction and legitimacy after structural barriers are removed (e.g., through alternative publishing platforms or direct policy engagement), reclassify as primarily structural. If self-doubt persists, partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making resistance harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in epistemic gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__credentialed_expertise_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1950, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(legi_tr_t1970, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(legi_tr_t1990, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(legi_tr_t2010, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(legi_tr_t2024, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(legi_be_t1950, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(legi_be_t1970, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(legi_be_t1990, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(legi_be_t2010, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(legi_be_t2024, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1950, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(legi_su_t1970, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(legi_su_t1990, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1990, 0.72).
narrative_ontology:measurement(legi_su_t2010, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 2010, 0.76).
narrative_ontology:measurement(legi_su_t2024, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__credentialed_expertise_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, public_health_policy_formation).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, climate_science_communication).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, educational_curriculum_design).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'legitimate_knowledge_boundary' kernel. Its high extractiveness and suppression contrast with the more inclusive 'experiential_pluralism_reading' and 'hybrid_coproduction_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
