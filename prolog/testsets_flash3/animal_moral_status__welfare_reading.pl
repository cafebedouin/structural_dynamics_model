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
 *   constraint_id: animal_moral_status__welfare_reading
 *   human_readable: Animal Welfare (Regulated Use) Constraint
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'welfare' reading of animal moral status,
 *   where animals are recognized as sentient beings whose suffering should be
 *   minimized, but their use by humans is still considered permissible under
 *   regulation. Cruelty is wrong, but use is not. This reading aims to
 *   balance human interests with animal interests, leading to a 'rope'
 *   classification with elements of 'tangled_rope' due to the inherent
 *   extraction from animals. The constraint focuses on the methods of use,
 *   not the use itself. This is one reading of the 'animal_moral_status'
 *   kernel; sibling readings include 'property_reading' and
 *   'abolitionist_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__welfare_reading, 0.35).
domain_priors:suppression_score(animal_moral_status__welfare_reading, 0.45).
domain_priors:theater_ratio(animal_moral_status__welfare_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__welfare_reading, rope).
narrative_ontology:human_readable(animal_moral_status__welfare_reading, "Animal Welfare (Regulated Use) Constraint").
narrative_ontology:topic_domain(animal_moral_status__welfare_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__welfare_reading, 'ace8e794-ff65-429c-982d-8db2565d0562').
narrative_ontology:cs_kernel_codification('ace8e794-ff65-429c-982d-8db2565d0562', formalized).
narrative_ontology:cs_authority_grounding('ace8e794-ff65-429c-982d-8db2565d0562', practice).
narrative_ontology:cs_interpretation_layer_present('ace8e794-ff65-429c-982d-8db2565d0562').
narrative_ontology:cs_reading_relation('ace8e794-ff65-429c-982d-8db2565d0562', animal_moral_status__property_reading, influences).
narrative_ontology:cs_reading_relation('ace8e794-ff65-429c-982d-8db2565d0562', animal_moral_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('ace8e794-ff65-429c-982d-8db2565d0562', foundational, sentience_implies_moral_consideration).
narrative_ontology:cs_axiom_status(sentience_implies_moral_consideration, holdable).
narrative_ontology:cs_axiom_grounding('ace8e794-ff65-429c-982d-8db2565d0562', sentience_implies_moral_consideration, deontological).
narrative_ontology:cs_axiom('ace8e794-ff65-429c-982d-8db2565d0562', foundational, human_use_of_animals_is_permissible).
narrative_ontology:cs_axiom_status(human_use_of_animals_is_permissible, holdable).
narrative_ontology:cs_axiom_grounding('ace8e794-ff65-429c-982d-8db2565d0562', human_use_of_animals_is_permissible, conventional).
narrative_ontology:cs_reference_frame('ace8e794-ff65-429c-982d-8db2565d0562', balanced_welfare_and_use).
narrative_ontology:cs_drift_state('ace8e794-ff65-429c-982d-8db2565d0562', contemporary_animal_rights_movement, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ace8e794-ff65-429c-982d-8db2565d0562', '').
narrative_ontology:cs_kernel_id(animal_moral_status__welfare_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, welfare_organizations).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, regulated_animal_industries).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, general_public).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, animals_in_regulated_use).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and define 'humane' standards, lobby for legislation, and monitor compliance. They gain legitimacy and funding by demonstrating progress in reducing animal suffering within existing systems. Their exit is constrained by the need to operate within the current legal and social framework.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, welfare_organizations, agenda_setter,
    organized, generational, constrained, national).

% Benefit from public acceptance and legal permission to continue animal use, provided they adhere to welfare standards. These standards, while adding costs, also provide a social license to operate and protect against more radical challenges. Exit means abandoning their business model.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulated_animal_industries, beneficiary,
    institutional, biographical, constrained, national).

% Benefits from a sense of ethical comfort that animal products and services are produced 'humanely,' without having to confront the full implications of animal sentience or the act of use itself. Can choose to consume less or different products, but generally accepts the framework.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, general_public, beneficiary,
    moderate, biographical, mobile, local).

% Are the direct subjects of the constraint, experiencing suffering that is minimized but not eliminated. Their interests are considered, but ultimately subordinated to human use. They have no agency or exit options within the system.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animals_in_regulated_use, payer,
    powerless, immediate, trapped, local).

% Reject the premise of regulated use, arguing that any use is a violation of animal rights. They are excluded from the mainstream welfare discourse, which they view as perpetuating the problem. Their identity is locked into a fundamental opposition to the current system.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, abolitionist_advocates, excluded,
    organized, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates societal expectations and industrial practices around animal use, ensuring a baseline of 'humane' treatment that allows industries to operate and the public to consume animal products with reduced ethical discomfort.
% TRANSFER_FUNCTION: Transfers a moral obligation to minimize suffering from individual consumers and industries to a set of regulated standards and enforcement mechanisms. It transfers the cost of 'humane' practices to industries (and indirectly to consumers) and transfers the burden of suffering to animals, albeit in a 'minimized' form.
% ABSENT_VOICES: Abolitionist advocates are largely absent from the policy-making table, as their fundamental rejection of animal use is incompatible with the welfare framework's premise of permissible use. They would argue that 'humane' use is an oxymoron.
% DISAPPEARANCE_RATIONALE: If the welfare constraint vanished, industries would face immediate and severe public backlash, potentially leading to widespread boycotts and legal challenges. The public would lose its ethical comfort, and the moral status of animals would become an unmediated, highly contentious issue, forcing a fundamental re-evaluation of human-animal relations.
% FOUNDING_PROBLEM: Unchecked cruelty and indifference towards animals, leading to widespread public revulsion and calls for basic protections, while still desiring to maintain animal use for human benefit.
% FOUNDING_PROBLEM_CORROBORATION: Welfare organizations and regulated industries attest that the problem of potential cruelty is always live and requires constant vigilance. Abolitionist advocates, while disagreeing with the solution, corroborate the historical problem of cruelty but argue the 'solution' is inadequate.
narrative_ontology:disappearance_verdict(animal_moral_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__welfare_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__welfare_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_moral_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__welfare_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate (0.35) because animals still experience suffering and loss of life, even if 'minimized.' Suppression (0.45) is present as the system actively suppresses alternatives to regulated use, such as abolition. Theater ratio (0.20) is low because welfare organizations genuinely work to reduce suffering, but some 'humane' claims may be performative. The constraint is claimed as a 'rope' because it genuinely coordinates a societal compromise, but its metrics lean towards 'tangled_rope' due to the inherent extraction from animals.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of welfare organizations and regulated industries, this is a functional rope that balances competing interests. From the perspective of the animals, it is a system that extracts their lives and well-being, albeit with some mitigation. Abolitionist advocates would see it as a tangled rope or snare, legitimizing exploitation.
 *
 * DIRECTIONALITY LOGIC:
 *   Welfare organizations and regulated industries are beneficiaries, gaining legitimacy and social license respectively. The general public benefits from ethical comfort. Animals in regulated use are the primary victims, bearing the costs of suffering and death. Abolitionist advocates are excluded, as their position fundamentally challenges the constraint's premise.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to minimize suffering is still live, as public awareness of animal sentience continues to grow. However, the 'permissible use' aspect is increasingly contested by abolitionist movements, suggesting a potential future mandatrophy if the underlying social contract shifts. The classification as a rope (with tangled elements) prevents mislabeling it as pure extraction, acknowledging the genuine coordination function, while still highlighting the costs borne by animals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suffering_quantification_ambiguity,
    'How can ''minimized suffering'' be objectively quantified and verified across diverse species and contexts, given the subjective nature of pain and distress?',
    'Development of more robust, species-specific behavioral and physiological indicators of welfare, combined with independent, transparent auditing of practices.',
    'If suffering is found to be consistently higher than ''minimized'' claims suggest, the extractiveness of the constraint would be re-evaluated upwards, potentially shifting its classification towards a snare. If robust minimization is demonstrated, the rope classification would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suffering_quantification_ambiguity, empirical, 'Uncertainty in objectively measuring and verifying ''minimized suffering'' in animals.').

omega_variable(
    use_permissibility_conceptual_boundary,
    'Is the concept of ''permissible use'' fundamentally compatible with the recognition of animal sentience and the moral imperative to minimize suffering, or does it create an inherent, unresolvable tension?',
    'Philosophical and ethical debate, potentially informed by shifts in societal values and scientific understanding of animal cognition. Resolution would be conceptual, not empirical.',
    'If deemed fundamentally incompatible, the ''rope'' aspect of coordination would be undermined, and the constraint would be reclassified as a ''tangled_rope'' or ''snare'' from the perspective of animal interests, as the coordination story would be seen as cover for extraction. If compatibility is affirmed, the rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(use_permissibility_conceptual_boundary, conceptual, 'Conceptual tension between animal sentience and permissible human use.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of abolitionist alternatives structural (legal/economic barriers) or internalized (societal acceptance of welfare framework)?',
    'Post-exit suppression trajectory: if abolitionist movements gain traction and legal avenues open, but public acceptance of welfare persists, reclassify as partially internalized. If legal/economic barriers are the primary block, it''s structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the public carries the suppression with them after exit from direct enforcement. If purely structural, removing barriers would lead to more rapid shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for abolitionist alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__welfare_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1970, animal_moral_status__welfare_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(anim_tr_t1985, animal_moral_status__welfare_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(anim_tr_t2000, animal_moral_status__welfare_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(anim_tr_t2010, animal_moral_status__welfare_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(anim_tr_t2024, animal_moral_status__welfare_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(anim_be_t1970, animal_moral_status__welfare_reading, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(anim_be_t1985, animal_moral_status__welfare_reading, base_extractiveness, 1985, 0.28).
narrative_ontology:measurement(anim_be_t2000, animal_moral_status__welfare_reading, base_extractiveness, 2000, 0.32).
narrative_ontology:measurement(anim_be_t2010, animal_moral_status__welfare_reading, base_extractiveness, 2010, 0.34).
narrative_ontology:measurement(anim_be_t2024, animal_moral_status__welfare_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1970, animal_moral_status__welfare_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(anim_su_t1985, animal_moral_status__welfare_reading, suppression_requirement, 1985, 0.38).
narrative_ontology:measurement(anim_su_t2000, animal_moral_status__welfare_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(anim_su_t2010, animal_moral_status__welfare_reading, suppression_requirement, 2010, 0.44).
narrative_ontology:measurement(anim_su_t2024, animal_moral_status__welfare_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__welfare_reading, attachment_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
