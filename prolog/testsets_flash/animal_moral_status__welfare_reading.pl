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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: animal_moral_status__welfare_reading
 *   human_readable: Animal Welfare Standard (Regulated Use)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This constraint, the 'welfare reading' of animal moral status, posits
 *   that animals are sentient beings whose suffering should be minimized
 *   within systems of regulated use. Cruelty is wrong, but use is
 *   permissible. It represents a dominant ethical and legal framework that
 *   seeks to balance human interests in animal use with a moral obligation to
 *   prevent unnecessary suffering. This reading is one of three major
 *   interpretations of the 'animal moral status' kernel, distinct from the
 *   'property reading' (animals as mere resources) and the 'abolitionist
 *   reading' (animals as rights-bearing individuals whose use is inherently
 *   wrong).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__welfare_reading, 0.35).
domain_priors:suppression_score(animal_moral_status__welfare_reading, 0.2).
domain_priors:theater_ratio(animal_moral_status__welfare_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__welfare_reading, rope).
narrative_ontology:human_readable(animal_moral_status__welfare_reading, "Animal Welfare Standard (Regulated Use)").
narrative_ontology:topic_domain(animal_moral_status__welfare_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__welfare_reading, '0d7f842b-2853-4133-b45e-e9c98fb75e39').
narrative_ontology:cs_kernel_codification('0d7f842b-2853-4133-b45e-e9c98fb75e39', formalized).
narrative_ontology:cs_authority_grounding('0d7f842b-2853-4133-b45e-e9c98fb75e39', practice).
narrative_ontology:cs_interpretation_layer_present('0d7f842b-2853-4133-b45e-e9c98fb75e39').
narrative_ontology:cs_reading_relation('0d7f842b-2853-4133-b45e-e9c98fb75e39', animal_moral_status__property_reading, influences).
narrative_ontology:cs_reading_relation('0d7f842b-2853-4133-b45e-e9c98fb75e39', animal_moral_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('0d7f842b-2853-4133-b45e-e9c98fb75e39', foundational, sentience_implies_moral_consideration).
narrative_ontology:cs_axiom_status(sentience_implies_moral_consideration, holdable).
narrative_ontology:cs_axiom_grounding('0d7f842b-2853-4133-b45e-e9c98fb75e39', sentience_implies_moral_consideration, deontological).
narrative_ontology:cs_axiom('0d7f842b-2853-4133-b45e-e9c98fb75e39', foundational, human_use_of_animals_is_permissible).
narrative_ontology:cs_axiom_status(human_use_of_animals_is_permissible, holdable).
narrative_ontology:cs_axiom_grounding('0d7f842b-2853-4133-b45e-e9c98fb75e39', human_use_of_animals_is_permissible, conventional).
narrative_ontology:cs_reference_frame('0d7f842b-2853-4133-b45e-e9c98fb75e39', balanced_use_and_welfare).
narrative_ontology:cs_drift_state('0d7f842b-2853-4133-b45e-e9c98fb75e39', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0d7f842b-2853-4133-b45e-e9c98fb75e39', '').
narrative_ontology:cs_kernel_id(animal_moral_status__welfare_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, animal_welfare_organizations).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, regulated_animal_industries).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, general_public).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, animals_in_regulated_use).
narrative_ontology:constraint_vindicates(animal_moral_status__welfare_reading, sentience_as_moral_consideration).
narrative_ontology:constraint_vindicates(animal_moral_status__welfare_reading, cruelty_is_wrong).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and enforce welfare standards within existing systems of animal use. They gain legitimacy and funding by demonstrating improvements in animal conditions, but their mandate is limited to minimizing suffering, not ending use.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animal_welfare_organizations, agenda_setter,
    organized, generational, constrained, national).

% Operate within welfare regulations, which provide social license and consumer trust. They bear some costs of compliance but benefit from public acceptance of their practices, which is contingent on perceived humane treatment.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulated_animal_industries, beneficiary,
    institutional, biographical, mobile, global).

% Benefits from the moral comfort of knowing animals are not subjected to 'cruelty' while still being able to consume animal products or services. Their support for welfare standards helps maintain the social contract around animal use.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, general_public, beneficiary,
    organized, biographical, mobile, national).

% Are the subjects of regulated use, experiencing suffering that is deemed 'acceptable' or 'minimized' within the system. Their interests are considered, but ultimately subordinated to human interests in use.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animals_in_regulated_use, payer,
    powerless, immediate, trapped, local).

% Advocate for the complete cessation of animal use, viewing welfare regulations as merely legitimizing exploitation. They are excluded from the core decision-making processes of the welfare framework, which focuses on improving conditions within use, not ending it.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, abolitionist_activists, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates societal expectations and industry practices regarding animal treatment, ensuring a baseline of 'humane' conditions that allows for continued animal use while addressing public moral concerns about suffering.
% TRANSFER_FUNCTION: Transfers moral comfort and social license to industries and consumers, in exchange for industries bearing the costs of welfare compliance and animals enduring 'minimized' suffering.
% ABSENT_VOICES: Abolitionist activists and the animals themselves are largely absent from the framing of 'regulated use.' Abolitionists would argue that the very concept of 'regulated use' is a moral compromise that perpetuates injustice, while animals cannot speak for their own interests beyond what humans interpret as suffering.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the social contract around animal use would collapse. Industries would face immediate public backlash and potential boycotts, leading to a crisis of legitimacy. Animal welfare organizations would lose their mandate, and the public would be forced to confront the direct moral implications of animal use without the buffer of 'humane' standards.
% FOUNDING_PROBLEM: The problem of widespread, unmitigated animal cruelty and public moral discomfort with animal suffering, alongside a desire to continue using animals for human benefit.
% FOUNDING_PROBLEM_CORROBORATION: Animal welfare organizations and the general public corroborate that the problem of potential cruelty remains live, requiring ongoing vigilance. Regulated industries also acknowledge the need for standards to maintain public trust. The problem of balancing use with minimizing suffering is widely accepted as an ongoing societal challenge.
narrative_ontology:disappearance_verdict(animal_moral_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__welfare_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__welfare_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_moral_status__welfare_reading, 'none', 1).

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
 *   The constraint is claimed as a Rope due to its genuine coordination function in establishing welfare standards and addressing public moral concerns. However, its extractiveness (0.35) is moderate because animals still bear the cost of 'minimized' suffering within systems of use, and the system actively suppresses alternatives (e.g., abolitionist views). The theater ratio (0.40) reflects that some 'welfare' practices are more about public relations and maintaining social license than genuinely prioritizing animal interests. Suppression (0.20) is low but present, as the framework actively marginalizes more radical views on animal liberation. Accessibility collapse (0.45) is moderate, as alternatives to animal use exist but are not widely adopted, and resistance (0.30) is moderate, primarily from abolitionist movements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of animal welfare organizations and regulated industries, this is a functional Rope that balances competing interests. From the perspective of animals themselves, and abolitionist activists, it functions more like a Tangled Rope or even a Snare, as it legitimizes and perpetuates their exploitation under the guise of 'humane' treatment. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Animal welfare organizations and the general public are beneficiaries, gaining moral legitimacy and comfort, respectively. Regulated animal industries are also beneficiaries, as the framework provides social license for their operations. Animals in regulated use are the primary victims, as their suffering, even if minimized, is still extracted. Abolitionist activists are excluded, as their core premise challenges the very foundation of 'regulated use.'
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suffering_minimization_vs_use_legitimation,
    'Is the primary effect of this constraint the minimization of animal suffering, or the legitimation of animal use?',
    'Empirical studies comparing animal welfare outcomes in regulated vs. unregulated systems, alongside analysis of public discourse shifts regarding animal use over time. If public acceptance of use increases disproportionately to actual welfare improvements, it suggests legitimation is the dominant function.',
    'If legitimation is the dominant function, the constraint''s effective extractiveness is higher, and its classification shifts closer to a Tangled Rope or Snare, as the coordination story serves as cover for continued extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suffering_minimization_vs_use_legitimation, empirical, 'Ambiguity in the constraint''s primary function: welfare improvement vs. use legitimation.').

omega_variable(
    welfare_vs_abolition_framing,
    'Is the ''welfare reading'' a necessary step towards abolition, or does it structurally entrench animal use by making it morally palatable?',
    'Longitudinal analysis of social movements and legislative outcomes: if welfare reforms consistently lead to further demands for abolition and eventual cessation of use, it''s a stepping stone. If they lead to stabilization of use and reduced pressure for abolition, it''s entrenchment.',
    'If it entrenches use, the constraint''s suppression of abolitionist alternatives is higher than measured, and its long-term classification leans more extractive. If it''s a stepping stone, its coordination function is more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_vs_abolition_framing, conceptual, 'Whether welfare reforms are a path to abolition or entrenchment of use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__welfare_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1970, animal_moral_status__welfare_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(anim_tr_t1985, animal_moral_status__welfare_reading, theater_ratio, 1985, 0.3).
narrative_ontology:measurement(anim_tr_t2000, animal_moral_status__welfare_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(anim_tr_t2010, animal_moral_status__welfare_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(anim_tr_t2024, animal_moral_status__welfare_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(anim_be_t1970, animal_moral_status__welfare_reading, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(anim_be_t1985, animal_moral_status__welfare_reading, base_extractiveness, 1985, 0.28).
narrative_ontology:measurement(anim_be_t2000, animal_moral_status__welfare_reading, base_extractiveness, 2000, 0.32).
narrative_ontology:measurement(anim_be_t2010, animal_moral_status__welfare_reading, base_extractiveness, 2010, 0.34).
narrative_ontology:measurement(anim_be_t2024, animal_moral_status__welfare_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1970, animal_moral_status__welfare_reading, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(anim_su_t1985, animal_moral_status__welfare_reading, suppression_requirement, 1985, 0.15).
narrative_ontology:measurement(anim_su_t2000, animal_moral_status__welfare_reading, suppression_requirement, 2000, 0.18).
narrative_ontology:measurement(anim_su_t2010, animal_moral_status__welfare_reading, suppression_requirement, 2010, 0.19).
narrative_ontology:measurement(anim_su_t2024, animal_moral_status__welfare_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__welfare_reading, identity_coordination).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'animal_moral_status' kernel. It is linked to the 'property_reading' and 'abolitionist_reading' as part of a constraint family, representing different interpretations of animal moral status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
