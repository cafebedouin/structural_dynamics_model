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
 *   This constraint represents the 'welfare reading' of animal moral status:
 *   animals are sentient beings whose suffering should be minimized within
 *   systems of regulated use, where cruelty is wrong but use is permissible.
 *   It focuses on the methods of use, not the use itself. This is one reading
 *   of the 'animal_moral_status' kernel, distinct from the 'property_reading'
 *   (animals as mere resources) and the 'abolitionist_reading' (all use is
 *   exploitation). The constraint aims to reduce suffering, but its core
 *   premise allows for continued animal use, leading to a low-to-moderate
 *   extractiveness for animals and a coordination function for human society.
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
narrative_ontology:cs_story_uid(animal_moral_status__welfare_reading, '619d9552-bc24-4613-9840-27d39f585a16').
narrative_ontology:cs_kernel_codification('619d9552-bc24-4613-9840-27d39f585a16', formalized).
narrative_ontology:cs_authority_grounding('619d9552-bc24-4613-9840-27d39f585a16', practice).
narrative_ontology:cs_interpretation_layer_present('619d9552-bc24-4613-9840-27d39f585a16').
narrative_ontology:cs_reading_relation('619d9552-bc24-4613-9840-27d39f585a16', animal_moral_status__property_reading, influences).
narrative_ontology:cs_reading_relation('619d9552-bc24-4613-9840-27d39f585a16', animal_moral_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('619d9552-bc24-4613-9840-27d39f585a16', foundational, sentience_implies_moral_consideration).
narrative_ontology:cs_axiom_status(sentience_implies_moral_consideration, holdable).
narrative_ontology:cs_axiom_grounding('619d9552-bc24-4613-9840-27d39f585a16', sentience_implies_moral_consideration, deontological).
narrative_ontology:cs_axiom('619d9552-bc24-4613-9840-27d39f585a16', foundational, human_use_of_animals_is_permissible).
narrative_ontology:cs_axiom_status(human_use_of_animals_is_permissible, holdable).
narrative_ontology:cs_axiom_grounding('619d9552-bc24-4613-9840-27d39f585a16', human_use_of_animals_is_permissible, conventional).
narrative_ontology:cs_reference_frame('619d9552-bc24-4613-9840-27d39f585a16', minimizing_suffering_within_use).
narrative_ontology:cs_drift_state('619d9552-bc24-4613-9840-27d39f585a16', contemporary_animal_rights_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('619d9552-bc24-4613-9840-27d39f585a16', '').
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

% Advocate for and define 'humane' standards, lobby for legislation, and monitor compliance. They gain legitimacy and funding by demonstrating progress in reducing animal suffering within existing systems. Their exit is constrained by their mission to improve conditions for animals within the current paradigm.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, welfare_organizations, agenda_setter,
    organized, generational, constrained, national).

% Operate within the framework of permissible animal use, benefiting from public acceptance and legal clarity. They bear the costs of compliance with welfare regulations but gain social license to operate. Exit means abandoning their business model or relocating to less regulated jurisdictions.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulated_animal_industries, beneficiary,
    institutional, biographical, constrained, national).

% Benefits from the moral comfort of knowing animal suffering is minimized in products they consume, without having to fundamentally alter their consumption patterns. Their engagement is often passive, but collective sentiment can influence policy.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, general_public, beneficiary,
    moderate, biographical, mobile, national).

% Are the direct subjects of regulated use, experiencing suffering that is minimized but not eliminated. Their interests are represented by welfare organizations, but they have no agency or exit options within the system. The constraint aims to reduce their suffering, but their use is still permissible.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animals_in_regulated_use, payer,
    powerless, immediate, trapped, local).

% Reject the premise of permissible animal use entirely, arguing that welfare regulations merely make exploitation more palatable. They are excluded from the core conversation of 'regulated use' because their position challenges the foundational assumption of the welfare reading.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, abolitionist_advocates, excluded,
    organized, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates human behavior to minimize animal suffering within systems of use, providing a framework for industries and consumers to operate with a degree of moral comfort and legal clarity.
% TRANSFER_FUNCTION: Transfers moral responsibility and potential guilt from human users to the regulatory framework, while transferring some resources (e.g., compliance costs) from industries to animal welfare measures. Animals bear the residual suffering.
% ABSENT_VOICES: Abolitionist advocates are largely absent from the 'regulated use' discussion, as their core premise (no use is permissible) is foreclosed by this reading. They would argue that the constraint legitimizes exploitation rather than ending suffering.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the moral landscape around animal use would become highly contested and unregulated. Industries would face public backlash, welfare organizations would lose their mandate, and animal suffering would likely increase dramatically without any legal or ethical framework to minimize it.
% FOUNDING_PROBLEM: Unchecked cruelty and indifference towards animal suffering in human practices, leading to widespread public discomfort and ethical concerns.
% FOUNDING_PROBLEM_CORROBORATION: Welfare organizations and the general public attest that the problem of animal suffering remains live, requiring ongoing vigilance and regulation. Regulated industries also acknowledge the need for standards to maintain social license. Abolitionist advocates, however, argue the problem is misidentified, and the true problem is use itself.
narrative_ontology:disappearance_verdict(animal_moral_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__welfare_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__welfare_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.35) because animals still experience suffering and loss of life, even under 'humane' conditions, but the constraint actively works to reduce the most egregious forms of harm. Suppression (0.45) reflects the active enforcement of welfare standards and the suppression of alternatives (e.g., abolitionist views) within the dominant discourse. Theater ratio (0.20) is present as some 'welfare' practices are more performative than genuinely beneficial to animals, but there is also real functional improvement. The claimed type is 'rope' because it genuinely coordinates efforts to reduce suffering, but with a 'tangled_rope' overlay due to the inherent extraction from animals.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of welfare organizations and the public, this constraint is a positive force, a 'rope' that coordinates efforts to reduce suffering. From the perspective of the animals, it is a system that, while mitigating harm, still permits their exploitation, making it feel more like a 'tangled_rope' or even a 'snare' of regulated suffering. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Welfare organizations and regulated industries are beneficiaries, as the constraint provides a framework for their operations and legitimacy. The general public also benefits from moral comfort. Animals in regulated use are the primary payers/victims, as they bear the direct costs of the system, even with minimized suffering. Abolitionist advocates are excluded, as their fundamental challenge to the premise of use places them outside the constraint's operational logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (minimizing suffering) remains live, but its scope (within systems of use) is increasingly contested. The classification as a 'rope' with a 'tangled_rope' overlay prevents mislabeling it as pure extraction, acknowledging its genuine coordination function, while also highlighting the inherent extraction from animals and the active enforcement required to maintain the 'use is permissible' premise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suffering_quantification_ambiguity,
    'How can ''minimized suffering'' be objectively quantified and verified across diverse species and contexts, given the subjective nature of sentience?',
    'Development of robust, cross-species behavioral and physiological indicators of stress and well-being, coupled with independent, transparent auditing of animal facilities.',
    'If suffering is found to be consistently higher than current ''minimized'' thresholds, the extractiveness of the constraint would be re-evaluated upward, potentially shifting its classification towards a Snare. If robust minimization is demonstrated, the Rope classification would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suffering_quantification_ambiguity, empirical, 'Uncertainty in objectively measuring and verifying ''minimized suffering'' in animals.').

omega_variable(
    use_permissibility_conceptual_boundary,
    'Is the premise ''use is permissible'' a foundational moral truth, or a socially constructed convention that could be revised?',
    'Philosophical and ethical discourse, shifts in societal values, and legal precedents that challenge or affirm the moral permissibility of animal use, independent of suffering minimization.',
    'If ''use is permissible'' is re-framed as a convention, the constraint''s legitimacy would be weakened, and its persistence would depend more heavily on active suppression of alternatives. If it is affirmed as a moral truth, the welfare reading''s foundation is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(use_permissibility_conceptual_boundary, conceptual, 'Ambiguity regarding the moral grounding of ''use is permissible'' as a core tenet of the welfare reading.').

omega_variable(
    welfare_vs_abolition_framing_conflict,
    'Does the ''welfare reading'' genuinely improve animal lives, or does it primarily serve to assuage human conscience and legitimize continued exploitation, as argued by abolitionists?',
    'Longitudinal studies comparing animal welfare outcomes under regulated use versus hypothetical abolitionist scenarios, alongside critical analysis of the psychological and social functions of welfare discourse for human consumers.',
    'If the primary function is found to be human moral comfort and legitimization, the theater_ratio would increase significantly, and the constraint''s classification would shift towards a Piton or Snare, as its stated coordination function (animal welfare) would be revealed as largely performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_vs_abolition_framing_conflict, preference, 'Conflict between welfare and abolitionist framings regarding the true impact and purpose of regulated animal use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__welfare_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__welfare_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anim_tr_t10, animal_moral_status__welfare_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(anim_tr_t20, animal_moral_status__welfare_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(anim_tr_t30, animal_moral_status__welfare_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__welfare_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(anim_tr_t50, animal_moral_status__welfare_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__welfare_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(anim_be_t10, animal_moral_status__welfare_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(anim_be_t20, animal_moral_status__welfare_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(anim_be_t30, animal_moral_status__welfare_reading, base_extractiveness, 30, 0.33).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__welfare_reading, base_extractiveness, 40, 0.34).
narrative_ontology:measurement(anim_be_t50, animal_moral_status__welfare_reading, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__welfare_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(anim_su_t10, animal_moral_status__welfare_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(anim_su_t20, animal_moral_status__welfare_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(anim_su_t30, animal_moral_status__welfare_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__welfare_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(anim_su_t50, animal_moral_status__welfare_reading, suppression_requirement, 50, 0.45).


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
