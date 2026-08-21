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
 *   human_readable: Animal Rights: Abolitionist Reading of Animal Status
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the abolitionist reading of the
 *   'animal_status' kernel, which posits that animals are rights-holders with
 *   inherent value, thereby precluding all instrumental use by humans. From
 *   this perspective, the existing global system of animal exploitation is a
 *   'snare' – a purely extractive arrangement maintained by active
 *   suppression and coercion, with no legitimate coordination function for
 *   the animals themselves. This reading rejects welfare reforms as merely
 *   legitimizing the underlying injustice.
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
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Animal Rights: Abolitionist Reading of Animal Status").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, 'cb4cf525-b995-4f74-a5ec-8bae745e675c').
narrative_ontology:cs_kernel_codification('cb4cf525-b995-4f74-a5ec-8bae745e675c', formalized).
narrative_ontology:cs_authority_grounding('cb4cf525-b995-4f74-a5ec-8bae745e675c', lineage).
narrative_ontology:cs_interpretation_layer_present('cb4cf525-b995-4f74-a5ec-8bae745e675c').
narrative_ontology:cs_reading_relation('cb4cf525-b995-4f74-a5ec-8bae745e675c', animal_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('cb4cf525-b995-4f74-a5ec-8bae745e675c', animal_status__welfare_reading, forecloses).
narrative_ontology:cs_axiom('cb4cf525-b995-4f74-a5ec-8bae745e675c', foundational, animal_sentience_inherent_value).
narrative_ontology:cs_axiom_status(animal_sentience_inherent_value, holdable).
narrative_ontology:cs_axiom_grounding('cb4cf525-b995-4f74-a5ec-8bae745e675c', animal_sentience_inherent_value, deontological).
narrative_ontology:cs_axiom('cb4cf525-b995-4f74-a5ec-8bae745e675c', foundational, instrumental_use_is_unjust).
narrative_ontology:cs_axiom_status(instrumental_use_is_unjust, holdable).
narrative_ontology:cs_axiom_grounding('cb4cf525-b995-4f74-a5ec-8bae745e675c', instrumental_use_is_unjust, deontological).
narrative_ontology:cs_reference_frame('cb4cf525-b995-4f74-a5ec-8bae745e675c', universal_moral_personhood).
narrative_ontology:cs_drift_state('cb4cf525-b995-4f74-a5ec-8bae745e675c', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('cb4cf525-b995-4f74-a5ec-8bae745e675c', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, human_users_of_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animals_as_rights_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% From the abolitionist perspective, animals are the primary victims, subjected to instrumental use across all domains (food, research, entertainment, labor) without consent or legal standing to assert their inherent value. Their lives, bodies, and labor are extracted.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animals_as_rights_holders, payer,
    powerless, immediate, trapped, universal).

% Industries (agriculture, research, entertainment) and individuals who benefit from the instrumental use of animals. They set the legal and social norms that permit and enforce animal exploitation, justifying it through various means (tradition, necessity, property rights).
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, human_users_of_animals, agenda_setter,
    institutional, generational, arbitrage, global).

% Individuals and organizations who actively challenge the instrumental use of animals, advocating for their status as rights-holders. They bear the costs of resistance (social, financial, legal) and work to change the foundational legal and ethical frameworks.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, abolitionist_advocates, observer,
    organized, generational, constrained, global).

% Individuals and organizations who seek to improve the conditions of animals within the existing framework of instrumental use. From the abolitionist perspective, their efforts, while well-intentioned, implicitly legitimize the fundamental injustice of animal exploitation, thus they are excluded from the core abolitionist conversation.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, welfarist_advocates, excluded,
    organized, biographical, constrained, global).

% The legislative and judicial bodies that codify and enforce the legal status of animals, primarily as property. They uphold the framework that permits instrumental use and suppress challenges to this status.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, legal_systems, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__abolitionist_reading, human_users_of_animals).
narrative_ontology:fixing_cost_class(animal_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The current system coordinates human access to and utilization of animal resources (food, labor, research, entertainment) by establishing and enforcing their status as property, thereby streamlining their instrumental use across various sectors.
% TRANSFER_FUNCTION: Transfers the lives, bodies, labor, and products of animals to humans for consumption, profit, and convenience, while externalizing the costs of suffering and ecological impact.
% ABSENT_VOICES: Animals themselves are structurally absent from any decision-making or legal discourse regarding their status. Future generations, who might adopt an abolitionist perspective, are also absent from current policy formation.
% DISAPPEARANCE_RATIONALE: If the constraint of animal instrumentalization vanished overnight, human society would undergo a profound and immediate restructuring of its food systems, scientific research, entertainment industries, and cultural practices. The global economy would be fundamentally altered, and ethical frameworks would shift dramatically.
% FOUNDING_PROBLEM: The arrangement was built to solve the problem of human desire for readily available animal products and services, and to establish a clear legal and social framework for human dominion over other species.
% FOUNDING_PROBLEM_CORROBORATION: Human users of animals and many cultural traditions attest that the 'problem' of human needs for animal resources is still live. Abolitionist advocates, however, contend that the 'problem' itself is illegitimate and that the arrangement persists as a moral failure, with corroboration from independent ethical philosophy and emerging scientific understanding of animal sentience.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.95) is near maximal because animals are deprived of their fundamental interests, including life and bodily autonomy, for human benefit. Suppression (0.98) is also near maximal, as animals are legally and physically prevented from resisting their instrumentalization, and their interests are entirely unrepresented in dominant legal systems. Theater ratio is low (0.05) because the system is highly functional in its extractive purpose; any performative aspects (e.g., 'humane' labeling) are seen as minor attempts to obscure the core extraction, not as a primary mode of persistence. Accessibility collapse is high (0.9) because animals have virtually no alternatives to instrumental use under the prevailing legal and social structures. Resistance (0.7) is substantial, reflecting the ongoing efforts of abolitionist movements.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist reading fundamentally diverges from the 'property_reading' and 'welfare_reading' by rejecting the permissibility of instrumental animal use entirely. While other readings might see coordination or limited extraction, this reading sees only pure extraction and suppression. The engine's classification of 'snare' from this perspective highlights the structural asymmetry and coercion inherent in the current system.
 *
 * DIRECTIONALITY LOGIC:
 *   From the abolitionist perspective, 'animals_as_rights_holders' are the full targets (victims) of the constraint, bearing the entire cost of instrumentalization. 'Human_users_of_animals' are the full beneficiaries and agenda-setters, deriving immense economic and social value from this arrangement. 'Abolitionist_advocates' act as observers and resistors, while 'welfarist_advocates' are structurally 'excluded' from the core abolitionist framing because their approach is seen as perpetuating the fundamental injustice.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_instrumental_use,
    'Is the instrumental use of animals fundamentally legitimate, or is it a moral wrong that should be abolished?',
    'Philosophical consensus shift, legal reform establishing animal personhood, or widespread societal adoption of abolitionist ethics.',
    'If instrumental use is deemed illegitimate, the constraint''s classification as a ''snare'' becomes universally accepted, and the ''beneficiaries'' of the current system are reclassified as perpetuators of injustice. If deemed legitimate, the abolitionist reading is foreclosed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_instrumental_use, conceptual, 'The foundational ethical question regarding animal status and use.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of animals primarily structural (legal/physical barriers) or internalized (e.g., through domestication and learned helplessness)?',
    'Ethological studies on animal agency and resistance in various contexts, and analysis of the persistence of ''wild'' behaviors in domesticated animals. Legal reforms that grant animals standing could also reveal latent resistance.',
    'If suppression is found to be significantly internalized, the effective suppression is even higher than the structural measure suggests, indicating a deeper entrenchment of the constraint. If purely structural, removing legal barriers would immediately enable greater resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for animals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1970, animal_status__abolitionist_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(anim_tr_t1980, animal_status__abolitionist_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(anim_tr_t1990, animal_status__abolitionist_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(anim_tr_t2000, animal_status__abolitionist_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(anim_tr_t2010, animal_status__abolitionist_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(anim_tr_t2020, animal_status__abolitionist_reading, theater_ratio, 2020, 0.05).

% Extraction over time
narrative_ontology:measurement(anim_be_t1970, animal_status__abolitionist_reading, base_extractiveness, 1970, 0.9).
narrative_ontology:measurement(anim_be_t1980, animal_status__abolitionist_reading, base_extractiveness, 1980, 0.92).
narrative_ontology:measurement(anim_be_t1990, animal_status__abolitionist_reading, base_extractiveness, 1990, 0.93).
narrative_ontology:measurement(anim_be_t2000, animal_status__abolitionist_reading, base_extractiveness, 2000, 0.94).
narrative_ontology:measurement(anim_be_t2010, animal_status__abolitionist_reading, base_extractiveness, 2010, 0.95).
narrative_ontology:measurement(anim_be_t2020, animal_status__abolitionist_reading, base_extractiveness, 2020, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1970, animal_status__abolitionist_reading, suppression_requirement, 1970, 0.95).
narrative_ontology:measurement(anim_su_t1980, animal_status__abolitionist_reading, suppression_requirement, 1980, 0.96).
narrative_ontology:measurement(anim_su_t1990, animal_status__abolitionist_reading, suppression_requirement, 1990, 0.97).
narrative_ontology:measurement(anim_su_t2000, animal_status__abolitionist_reading, suppression_requirement, 2000, 0.97).
narrative_ontology:measurement(anim_su_t2010, animal_status__abolitionist_reading, suppression_requirement, 2010, 0.98).
narrative_ontology:measurement(anim_su_t2020, animal_status__abolitionist_reading, suppression_requirement, 2020, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
