% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__abolitionist_reading, []).

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
 *   constraint_id: animal_status__abolitionist_reading
 *   human_readable: Abolitionist Reading: Animals as Rights-Holders Precluding Instrumental Use
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.95).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.98).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Abolitionist Reading: Animals as Rights-Holders Precluding Instrumental Use").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, '7281542c-23c9-4704-a393-0de8e9a465f1').
narrative_ontology:cs_kernel_codification('7281542c-23c9-4704-a393-0de8e9a465f1', formalized).
narrative_ontology:cs_authority_grounding('7281542c-23c9-4704-a393-0de8e9a465f1', extraction).
narrative_ontology:cs_interpretation_layer_present('7281542c-23c9-4704-a393-0de8e9a465f1').
narrative_ontology:cs_reading_relation('7281542c-23c9-4704-a393-0de8e9a465f1', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('7281542c-23c9-4704-a393-0de8e9a465f1', animal_status__property_reading, forecloses).
narrative_ontology:cs_axiom('7281542c-23c9-4704-a393-0de8e9a465f1', foundational, animals_are_rights_holders).
narrative_ontology:cs_axiom_status(animals_are_rights_holders, holdable).
narrative_ontology:cs_axiom_grounding('7281542c-23c9-4704-a393-0de8e9a465f1', animals_are_rights_holders, deontological).
narrative_ontology:cs_axiom('7281542c-23c9-4704-a393-0de8e9a465f1', foundational, instrumental_use_is_unjust).
narrative_ontology:cs_axiom_status(instrumental_use_is_unjust, holdable).
narrative_ontology:cs_axiom_grounding('7281542c-23c9-4704-a393-0de8e9a465f1', instrumental_use_is_unjust, deontological).
narrative_ontology:cs_reference_frame('7281542c-23c9-4704-a393-0de8e9a465f1', universal_moral_consideration).
narrative_ontology:cs_drift_state('7281542c-23c9-4704-a393-0de8e9a465f1', contemporary_legal_framework, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('7281542c-23c9-4704-a393-0de8e9a465f1', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, animal_use_industries).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, consumers_of_animal_products).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animals_used_instrumentally).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, animal_rights_doctrine).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, inherent_value_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the direct targets of instrumental use, they bear the full cost of extraction, including suffering, confinement, and death. They have no legal standing to object and no means of exit from the system of use.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animals_used_instrumentally, payer,
    powerless, immediate, trapped, global).

% Benefit directly from the instrumental use of animals, deriving economic value from their bodies and labor. They actively lobby against changes to animal legal status and promote narratives that justify current practices. Their business models are predicated on the current legal status of animals.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_use_industries, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the availability and affordability of animal products and services. While they may not directly enforce the constraint, their demand sustains the industries that do. They have the option to choose plant-based alternatives but are not compelled to do so.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, consumers_of_animal_products, beneficiary,
    moderate, biographical, mobile, global).

% Actively challenge the instrumental use of animals, advocating for their legal status as rights-holders. They document the harms of animal use and work to dismantle the systems that perpetuate it. From their perspective, the current system is a snare.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_abolitionist_advocates, observer,
    organized, generational, analytical, global).

% Seek to improve the conditions of animals within the existing framework of instrumental use. From the abolitionist reading, their efforts are seen as legitimizing the fundamental injustice of animal use, rather than challenging it, and thus they are excluded from the abolitionist's core project.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, welfare_reform_advocates, excluded,
    organized, biographical, constrained, global).

% Codify and enforce the legal status of animals as property, facilitating their instrumental use. They provide the framework within which animal use industries operate and against which abolitionist advocates struggle. Changing this framework requires significant legal and political shifts.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, legal_systems, agenda_setter,
    institutional, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__abolitionist_reading, animal_use_industries).
narrative_ontology:fixing_cost_class(animal_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The current system coordinates the efficient production and distribution of animal products by defining animals as property, thereby removing legal and ethical barriers to their instrumental use.
% TRANSFER_FUNCTION: Transfers the inherent value and bodily autonomy of animals to human users and industries, enabling the extraction of resources (meat, dairy, eggs, labor, research subjects) from them.
% ABSENT_VOICES: Animals themselves are structurally absent from any legal or ethical discourse that would grant them standing. Their interests are represented by advocates, but they cannot speak for themselves. Future generations, who might inherit a world with different ethical norms regarding animals, are also absent.
% DISAPPEARANCE_RATIONALE: If the constraint of animals as property vanished overnight, the entire animal agriculture, research, and entertainment industries would collapse. Legal systems would need to be fundamentally reconfigured to recognize animal rights, leading to a massive societal and economic reorganization.
% FOUNDING_PROBLEM: The problem of how to efficiently utilize animals for human benefit and sustenance, and how to manage human-animal interactions within a framework of human dominion.
% FOUNDING_PROBLEM_CORROBORATION: The animal use industries and many consumers attest that the problem of human sustenance and resource needs is still live, requiring animal use. Animal abolitionist advocates, however, argue that the 'problem' is a construct of human-centric ethics and that the current arrangement is a moral failure, not a solution to a genuine problem. Independent ethical philosophers and some scientists corroborate the abolitionist view that the problem is reframed by a deontological commitment to animal rights.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__abolitionist_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_personhood_status,
    'Would granting legal personhood to animals fundamentally alter the constraint''s structure, or would new forms of instrumentalization emerge?',
    'Comparative legal analysis of jurisdictions with varying degrees of animal legal standing, and empirical observation of the emergence of new forms of exploitation.',
    'If legal personhood genuinely dismantles instrumental use, the constraint''s extractiveness and suppression would drop dramatically. If new forms of exploitation emerge, the constraint might reclassify as a different type of snare or tangled rope, with different mechanisms of extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_personhood_status, empirical, 'Uncertainty about the efficacy of legal personhood in ending instrumental animal use.').

omega_variable(
    welfare_reform_legitimation,
    'Does the pursuit of animal welfare reforms inadvertently legitimize the underlying instrumental use of animals, or do they represent a genuine step towards reducing suffering?',
    'Longitudinal studies of public perception and industry practices in response to welfare reforms, and philosophical analysis of the ethical implications of incremental change versus abolition.',
    'If welfare reforms primarily serve to legitimize the system, the abolitionist reading''s assessment of high extraction and suppression remains valid. If they genuinely reduce suffering without legitimizing, the ''welfare_reading'' might gain more ethical weight, potentially influencing the ''coexists_with'' relation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_legitimation, conceptual, 'Ambiguity regarding the ethical impact of animal welfare reforms from an abolitionist perspective.').

omega_variable(
    kernel_reading_divergence,
    'Is the abolitionist reading of animal status fundamentally irreconcilable with the property and welfare readings, or can a broader ethical framework encompass all three?',
    'Philosophical and legal discourse attempting to construct a unified framework, and observation of whether such frameworks gain widespread acceptance or remain contested.',
    'If irreconcilable, the ''forecloses'' relation to the property reading is strengthened. If a broader framework emerges, the relations might shift to ''coexists_with'' or ''influences'', indicating a more complex, multi-layered ethical landscape.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'The fundamental conceptual divergence between the abolitionist, property, and welfare readings of animal status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1970, animal_status__abolitionist_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(anim_tr_t1980, animal_status__abolitionist_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(anim_tr_t1990, animal_status__abolitionist_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(anim_tr_t2000, animal_status__abolitionist_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(anim_tr_t2010, animal_status__abolitionist_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(anim_tr_t2024, animal_status__abolitionist_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(anim_be_t1970, animal_status__abolitionist_reading, base_extractiveness, 1970, 0.9).
narrative_ontology:measurement(anim_be_t1980, animal_status__abolitionist_reading, base_extractiveness, 1980, 0.92).
narrative_ontology:measurement(anim_be_t1990, animal_status__abolitionist_reading, base_extractiveness, 1990, 0.93).
narrative_ontology:measurement(anim_be_t2000, animal_status__abolitionist_reading, base_extractiveness, 2000, 0.94).
narrative_ontology:measurement(anim_be_t2010, animal_status__abolitionist_reading, base_extractiveness, 2010, 0.95).
narrative_ontology:measurement(anim_be_t2024, animal_status__abolitionist_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1970, animal_status__abolitionist_reading, suppression_requirement, 1970, 0.9).
narrative_ontology:measurement(anim_su_t1980, animal_status__abolitionist_reading, suppression_requirement, 1980, 0.92).
narrative_ontology:measurement(anim_su_t1990, animal_status__abolitionist_reading, suppression_requirement, 1990, 0.94).
narrative_ontology:measurement(anim_su_t2000, animal_status__abolitionist_reading, suppression_requirement, 2000, 0.96).
narrative_ontology:measurement(anim_su_t2010, animal_status__abolitionist_reading, suppression_requirement, 2010, 0.97).
narrative_ontology:measurement(anim_su_t2024, animal_status__abolitionist_reading, suppression_requirement, 2024, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, factory_farming_regulations).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_testing_protocols).

% DUAL FORMULATION NOTE:
% This constraint is the abolitionist reading of the 'animal_status' kernel. It is structurally distinct from the 'welfare_reading' and 'property_reading' due to fundamental differences in ethical premises and victim identification. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
