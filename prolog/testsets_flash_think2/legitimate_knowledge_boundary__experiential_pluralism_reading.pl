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
 *   human_readable: Experiential Pluralism in Knowledge Legitimation
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'experiential pluralism' reading of the
 *   legitimate knowledge boundary, asserting that knowledge derived from
 *   lived experience and community validation is legitimate, with
 *   methodological standards serving as one tool among many. It challenges
 *   traditional hierarchies that privilege credentialed expertise. The
 *   metrics reflect a low-extraction, low-suppression constraint, consistent
 *   with a 'rope' classification, as it aims to coordinate diverse knowledge
 *   forms rather than extract from them.
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
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__experiential_pluralism_reading, rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__experiential_pluralism_reading, "Experiential Pluralism in Knowledge Legitimation").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__experiential_pluralism_reading, "epistemology/science_and_technology_studies/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__experiential_pluralism_reading, 'e05129a7-b45c-4c3c-9b0e-5aca12a1be2c').
narrative_ontology:cs_kernel_codification('e05129a7-b45c-4c3c-9b0e-5aca12a1be2c', distributed).
narrative_ontology:cs_authority_grounding('e05129a7-b45c-4c3c-9b0e-5aca12a1be2c', practice).
narrative_ontology:cs_interpretation_layer_present('e05129a7-b45c-4c3c-9b0e-5aca12a1be2c').
narrative_ontology:cs_reading_relation('e05129a7-b45c-4c3c-9b0e-5aca12a1be2c', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('e05129a7-b45c-4c3c-9b0e-5aca12a1be2c', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('e05129a7-b45c-4c3c-9b0e-5aca12a1be2c', foundational, experiential_validity_is_primary).
narrative_ontology:cs_axiom_status(experiential_validity_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('e05129a7-b45c-4c3c-9b0e-5aca12a1be2c', experiential_validity_is_primary, empirically_contingent).
narrative_ontology:cs_axiom('e05129a7-b45c-4c3c-9b0e-5aca12a1be2c', foundational, community_validation_is_sufficient).
narrative_ontology:cs_axiom_status(community_validation_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('e05129a7-b45c-4c3c-9b0e-5aca12a1be2c', community_validation_is_sufficient, conventional).
narrative_ontology:cs_reference_frame('e05129a7-b45c-4c3c-9b0e-5aca12a1be2c', decentralized_epistemic_commons).
narrative_ontology:cs_drift_state('e05129a7-b45c-4c3c-9b0e-5aca12a1be2c', contemporary_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e05129a7-b45c-4c3c-9b0e-5aca12a1be2c', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, experiential_knowledge_holders).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, citizen_scientists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, academic_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals whose lived experiences are recognized as a primary source of legitimate knowledge. They benefit from the validation of their insights and the reduced barriers to epistemic participation.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, experiential_knowledge_holders, beneficiary,
    moderate, biographical, mobile, local).

% Groups or networks that collectively validate knowledge claims based on shared experience and local context. They gain authority in defining what counts as legitimate knowledge within their domains.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, community_validators, agenda_setter,
    organized, biographical, mobile, regional).

% Academics and professionals whose authority traditionally derives from formal training and methodological rigor. Under this reading, their claims are re-situated as one form of knowledge among many, potentially diminishing their exclusive epistemic power.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts, payer,
    powerful, biographical, constrained, global).

% Universities and research bodies that traditionally gatekeep knowledge production and validation. This reading challenges their monopoly on legitimacy, requiring them to adapt to more pluralistic epistemic frameworks.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, academic_institutions, payer,
    institutional, generational, constrained, national).

% Groups historically excluded from mainstream knowledge production. They benefit from the recognition of their unique perspectives and the empowerment to define their own knowledge boundaries.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_communities, beneficiary,
    organized, generational, identity_locked, local).

% Scholars and analysts who study the dynamics of knowledge production and legitimation, observing the contestation between different epistemic frameworks.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse forms of knowledge, ensuring that experiential and community-validated insights are integrated into broader understandings, preventing epistemic exclusion and fostering more holistic problem-solving.
% TRANSFER_FUNCTION: Transfers epistemic authority and validation power from centralized, credentialed bodies to distributed communities and individuals with lived experience, rebalancing the hierarchy of knowledge legitimation.
% ABSENT_VOICES: Voices that prioritize universal, decontextualized methodological rigor above all else, or those who benefit from the existing hierarchy of knowledge, would object. They are often found within established academic and scientific institutions, or among those who fear a 'relativist' erosion of objective truth.
% DISAPPEARANCE_RATIONALE: If this framework for knowledge legitimation vanished overnight, the epistemic landscape would likely revert to more hierarchical, exclusionary forms, where credentialed, institutionally-backed expertise would regain its unchallenged dominance, marginalizing experiential and community-based knowledge once again.
% FOUNDING_PROBLEM: The historical exclusion and invalidation of knowledge from marginalized communities and non-academic sources, leading to incomplete, biased, or unjust understandings of complex social and environmental problems.
% FOUNDING_PROBLEM_CORROBORATION: Social justice movements, indigenous rights advocates, and critical scholars in Science and Technology Studies (STS) and decolonial studies consistently corroborate the persistence of epistemic injustice and the need for pluralistic knowledge frameworks. This corroboration comes from outside the direct beneficiaries of this specific reading.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__experiential_pluralism_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__experiential_pluralism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The low extractiveness (0.25) and suppression (0.15) reflect this reading's goal of democratizing knowledge and reducing barriers to participation. It is a 'rope' because it seeks to coordinate diverse epistemic inputs for collective benefit, rather than imposing a single, extractive standard. The moderate resistance (0.40) acknowledges the ongoing contestation from established epistemic authorities. The decreasing trend in extractiveness, theater, and suppression over the interval (1990-2020) reflects the growing influence and acceptance of this reading within certain academic and activist circles.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of experiential knowledge holders, this framework is a liberating coordination mechanism. From the perspective of traditional credentialed experts, it may be perceived as a threat to rigor or a dilution of standards, leading to a sense of 'loss' of epistemic authority, even if no direct extraction occurs.
 *
 * DIRECTIONALITY LOGIC:
 *   Experiential knowledge holders and marginalized communities are clear beneficiaries, as their knowledge is validated and empowered (low directionality). Credentialed experts and academic institutions, while not 'victims' in the extractive sense, bear the 'cost' of re-evaluating their epistemic authority and adapting to a more pluralistic landscape (higher directionality, but not full target). Community validators act as agenda-setters, defining local standards.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling genuine efforts to coordinate diverse knowledge forms as pure extraction. While it challenges existing power structures, its primary function is to enable broader participation and more inclusive knowledge, not to extract rents. The 'rope' classification acknowledges its coordination function, while the 'payer' roles for traditional experts highlight the rebalancing of epistemic authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marginal_cost_of_pluralism,
    'What is the actual marginal cost (e.g., in terms of coherence, generalizability, or decision-making speed) of integrating radically pluralistic knowledge forms, and who bears this cost?',
    'Empirical studies of interdisciplinary and transdisciplinary collaborations that successfully integrate diverse knowledge, assessing their efficiency and effectiveness compared to monodisciplinary approaches.',
    'If the costs are high and disproportionately borne by certain groups, the ''rope'' classification might be too optimistic, suggesting hidden extraction or coordination failures. If costs are manageable and benefits widely distributed, the ''rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_cost_of_pluralism, empirical, 'The practical costs and distribution of burdens associated with implementing epistemic pluralism.').

omega_variable(
    epistemic_authority_redefinition,
    'To what extent does this reading genuinely redefine epistemic authority versus merely adding new categories of ''legitimate'' knowledge without fundamentally altering power dynamics?',
    'Longitudinal studies tracking changes in funding allocation, publication patterns, and policy influence for experiential vs. credentialed knowledge over time. Analysis of whether ''experiential'' knowledge is tokenized or genuinely integrated.',
    'If power dynamics remain largely unchanged, the ''beneficiary'' roles for marginalized groups might be overstated, and the constraint''s actual impact on epistemic justice would be lower than claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_authority_redefinition, empirical, 'The depth of redefinition of epistemic authority vs. superficial inclusion.').

omega_variable(
    kernel_framing_ambiguity,
    'Is this constraint a genuine ''rope'' coordinating diverse knowledge, or is it a ''snare'' for traditional expertise, using ''pluralism'' as cover to dismantle established standards?',
    'Analysis of the outcomes of policy decisions and resource allocations made under this framework: do they lead to more equitable and effective solutions, or do they primarily serve to undermine existing institutions without clear benefit?',
    'If the latter, the ''claimed_type'' of rope would be a misdirection, and the constraint would reclassify as a snare from the perspective of credentialed experts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the pluralism framework is genuinely coordinative or covertly extractive from traditional expertise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__experiential_pluralism_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1990, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(legi_tr_t1996, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 1996, 0.13).
narrative_ontology:measurement(legi_tr_t2002, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 2002, 0.12).
narrative_ontology:measurement(legi_tr_t2008, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 2008, 0.11).
narrative_ontology:measurement(legi_tr_t2014, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(legi_tr_t2020, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(legi_be_t1990, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(legi_be_t1996, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 1996, 0.28).
narrative_ontology:measurement(legi_be_t2002, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 2002, 0.27).
narrative_ontology:measurement(legi_be_t2008, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 2008, 0.26).
narrative_ontology:measurement(legi_be_t2014, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 2014, 0.25).
narrative_ontology:measurement(legi_be_t2020, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 2020, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1990, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(legi_su_t1996, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 1996, 0.18).
narrative_ontology:measurement(legi_su_t2002, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 2002, 0.17).
narrative_ontology:measurement(legi_su_t2008, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 2008, 0.16).
narrative_ontology:measurement(legi_su_t2014, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 2014, 0.15).
narrative_ontology:measurement(legi_su_t2020, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 2020, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__experiential_pluralism_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, scientific_funding_allocation).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, public_health_policy_formation).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, environmental_justice_advocacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
