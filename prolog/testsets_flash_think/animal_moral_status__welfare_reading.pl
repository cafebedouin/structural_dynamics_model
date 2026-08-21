% ============================================================================
% CONSTRAINT STORY: animal_moral_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__welfare_reading, []).

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
 *   constraint_id: animal_moral_status__welfare_reading
 *   human_readable: Animal Welfare within Regulated Use
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the 'welfare_reading' of the
 *   'animal_moral_status' kernel, which posits that animals are sentient
 *   beings deserving of minimized suffering within systems of regulated human
 *   use. It stands in contrast to the 'property_reading' (animals as mere
 *   resources) and the 'abolitionist_reading' (animals as rights-bearing
 *   individuals). The constraint aims to balance human interests in animal
 *   use with moral concerns about animal suffering, leading to a system of
 *   'humane' exploitation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__welfare_reading, 0.45).
domain_priors:suppression_score(animal_moral_status__welfare_reading, 0.6).
domain_priors:theater_ratio(animal_moral_status__welfare_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_moral_status__welfare_reading, "Animal Welfare within Regulated Use").
narrative_ontology:topic_domain(animal_moral_status__welfare_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__welfare_reading, 'be099a1b-d2fc-482b-bff3-8b834a2c3ea6').
narrative_ontology:cs_kernel_codification('be099a1b-d2fc-482b-bff3-8b834a2c3ea6', formalized).
narrative_ontology:cs_authority_grounding('be099a1b-d2fc-482b-bff3-8b834a2c3ea6', lineage).
narrative_ontology:cs_interpretation_layer_present('be099a1b-d2fc-482b-bff3-8b834a2c3ea6').
narrative_ontology:cs_reading_relation('be099a1b-d2fc-482b-bff3-8b834a2c3ea6', animal_moral_status__property_reading, influences).
narrative_ontology:cs_reading_relation('be099a1b-d2fc-482b-bff3-8b834a2c3ea6', animal_moral_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('be099a1b-d2fc-482b-bff3-8b834a2c3ea6', foundational, animal_sentience_matters).
narrative_ontology:cs_axiom_status(animal_sentience_matters, holdable).
narrative_ontology:cs_axiom_grounding('be099a1b-d2fc-482b-bff3-8b834a2c3ea6', animal_sentience_matters, deontological).
narrative_ontology:cs_axiom('be099a1b-d2fc-482b-bff3-8b834a2c3ea6', foundational, human_use_is_permissible).
narrative_ontology:cs_axiom_status(human_use_is_permissible, holdable).
narrative_ontology:cs_axiom_grounding('be099a1b-d2fc-482b-bff3-8b834a2c3ea6', human_use_is_permissible, conventional).
narrative_ontology:cs_reference_frame('be099a1b-d2fc-482b-bff3-8b834a2c3ea6', utilitarian_welfare_calculus).
narrative_ontology:cs_drift_state('be099a1b-d2fc-482b-bff3-8b834a2c3ea6', contemporary_animal_rights_movement, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('be099a1b-d2fc-482b-bff3-8b834a2c3ea6', '').
narrative_ontology:cs_kernel_id(animal_moral_status__welfare_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, welfare_organizations).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, regulated_industries).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, consumers).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, animals_in_regulated_use).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, scientific_researchers).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, regulated_industries).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, scientific_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and help define 'humane' standards, gaining legitimacy and funding by working within the framework of regulated use. They benefit from the existence of a system that acknowledges animal suffering, even if it doesn't eliminate use.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, welfare_organizations, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, welfare_organizations, beneficiary).

% Benefit from social license to operate and public comfort with their products, which is enabled by welfare regulations. They bear the costs of compliance, which can be substantial, but avoid more radical challenges to animal use.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulated_industries, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, regulated_industries, payer).

% Benefit from access to animal products and services with reduced moral guilt, believing that the animals involved are treated 'humanely'. They can choose plant-based alternatives but often prefer animal products.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, consumers, beneficiary,
    moderate, immediate, mobile, local).

% Bear the suffering inherent in systems of regulated use, even when 'minimized'. Their interests are considered, but ultimately subordinated to human interests in use. They have no agency or exit options within this framework.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animals_in_regulated_use, payer,
    powerless, immediate, trapped, local).

% Are structurally excluded from the core conversation of 'regulated use' because their premise (animals as rights-bearing individuals, no use permissible) fundamentally challenges the constraint's foundation. They operate outside this framework, seeking its overthrow.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, abolitionist_advocates, excluded,
    organized, generational, identity_locked, global).

% Enforces animal welfare laws and regulations, adjudicating disputes and setting precedents for what constitutes 'cruelty' and 'permissible use'. It provides the formal structure for the constraint.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, legal_system, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from the ability to use animals in research, but must adhere to strict ethical guidelines and welfare protocols. They bear the costs of compliance and ethical review, which can be significant.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, scientific_researchers, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, scientific_researchers, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates human moral concern for animal suffering with the desire for animal products and services, by establishing acceptable limits and methods of use, thereby legitimizing continued animal exploitation under a 'humane' banner.
% TRANSFER_FUNCTION: Transfers moral comfort and social license to human consumers and industries, in exchange for animals bearing suffering within regulated limits, and transfers resources to welfare organizations for their advocacy and oversight.
% ABSENT_VOICES: Abolitionist advocates, who would argue against any use, and the animals themselves, whose suffering is mediated through human interpretation and regulation.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, either animal use would become unregulated and potentially more cruel (leading to significant public outcry and moral crisis), or it would collapse entirely under abolitionist pressure, fundamentally altering industries, human diets, and scientific practices. The current equilibrium would be lost.
% FOUNDING_PROBLEM: Unchecked cruelty to animals, leading to public moral discomfort and calls for intervention, while maintaining the economic and cultural benefits of animal use.
% FOUNDING_PROBLEM_CORROBORATION: Public opinion polls consistently show concern for animal welfare alongside demand for animal products. Legislative debates and the ongoing work of animal welfare organizations attest to the persistent tension between use and suffering minimization. Industry reports on consumer demand for 'humane' products also corroborate the need for this mediating framework.
narrative_ontology:disappearance_verdict(animal_moral_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__welfare_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__welfare_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(animal_moral_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__welfare_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__welfare_reading_tests).
:- end_tests(animal_moral_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates human moral sensibilities with the desire for animal products (minimizing suffering for human comfort), but simultaneously extracts suffering from animals within 'permissible' limits. Base extractiveness (0.45) reflects the ongoing cost to animals, while suppression (0.6) indicates the active enforcement of regulations that limit alternatives for animals and exclude more radical moral claims. Theater ratio (0.4) suggests a significant portion of 'welfare' activity is performative, reassuring the public without fundamentally altering the extractive relationship. The initial decrease in extractiveness reflects early welfare improvements, followed by a slight increase as 'humane' use expanded and became more entrenched.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of welfare organizations and consumers, the constraint is a net positive, reducing suffering and enabling morally acceptable use. From the perspective of animals, it is a system of regulated suffering. Abolitionist advocates view it as a sophisticated form of extraction, legitimizing violence under the guise of 'humane' treatment.
 *
 * DIRECTIONALITY LOGIC:
 *   Welfare organizations, regulated industries, and consumers are beneficiaries, gaining legitimacy, social license, and guilt-reduced products, respectively. Animals in regulated use are the primary victims, bearing the costs of suffering. The legal system and scientific researchers act as agenda-setters and beneficiaries within the framework. Abolitionist advocates are excluded, as their core premise challenges the very foundation of 'regulated use'.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (minimizing suffering while permitting use) remains live, as evidenced by ongoing public debate and industry efforts. Mandatrophy is unlikely because the underlying tension it manages is persistent. However, the balance between 'minimization' and 'permissibility' is constantly contested, leading to shifts in extractiveness and theater over time rather than outright atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_welfare_vs_social_license,
    'To what extent do animal welfare regulations genuinely reduce animal suffering, versus primarily serving to provide social license for continued animal use?',
    'Independent, longitudinal studies comparing animal physiological and behavioral indicators under regulated vs. unregulated conditions, and public perception surveys on the impact of welfare labeling on consumer purchasing behavior.',
    'If regulations are found to be largely performative (high theater, low actual suffering reduction), the constraint''s effective extractiveness would be higher, and its coordination function would be reclassified as primarily legitimizing exploitation rather than genuinely mitigating harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_welfare_vs_social_license, empirical, 'Assessing the true impact of welfare regulations on animals versus human moral comfort.').

omega_variable(
    moral_status_boundary_ambiguity,
    'Is the ''sentient being'' status a stable moral category, or is it a temporary compromise between ''property'' and ''rights-holder'' that is inherently unstable?',
    'Analysis of legal and philosophical trends: if the ''sentient being'' category consistently drifts towards either full property or full rights, it indicates an unstable equilibrium. If it persists as a distinct, coherent category, it suggests stability.',
    'If unstable, the constraint is a temporary Scaffold or a contested Tangled Rope, constantly under pressure to reclassify towards either the property_reading (Snare) or the abolitionist_reading (Rope/Mountain of rights). If stable, its current Tangled Rope classification is robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_status_boundary_ambiguity, conceptual, 'Stability of the ''sentient being'' moral category.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__welfare_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1970, animal_moral_status__welfare_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(anim_tr_t1980, animal_moral_status__welfare_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(anim_tr_t1990, animal_moral_status__welfare_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(anim_tr_t2000, animal_moral_status__welfare_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(anim_tr_t2010, animal_moral_status__welfare_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(anim_tr_t2020, animal_moral_status__welfare_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(anim_be_t1970, animal_moral_status__welfare_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(anim_be_t1980, animal_moral_status__welfare_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(anim_be_t1990, animal_moral_status__welfare_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(anim_be_t2000, animal_moral_status__welfare_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(anim_be_t2010, animal_moral_status__welfare_reading, base_extractiveness, 2010, 0.43).
narrative_ontology:measurement(anim_be_t2020, animal_moral_status__welfare_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1970, animal_moral_status__welfare_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(anim_su_t1980, animal_moral_status__welfare_reading, suppression_requirement, 1980, 0.48).
narrative_ontology:measurement(anim_su_t1990, animal_moral_status__welfare_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(anim_su_t2000, animal_moral_status__welfare_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(anim_su_t2010, animal_moral_status__welfare_reading, suppression_requirement, 2010, 0.59).
narrative_ontology:measurement(anim_su_t2020, animal_moral_status__welfare_reading, suppression_requirement, 2020, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__welfare_reading, identity_coordination).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__abolitionist_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
