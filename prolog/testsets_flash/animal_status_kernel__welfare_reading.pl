% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__welfare_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: animal_status_kernel__welfare_reading
 *   human_readable: Animal Welfare Obligations (Welfare Reading)
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint represents the 'welfare' reading of animal status, where
 *   animals are recognized as sentient beings whose suffering is morally
 *   relevant, but their use by humans is acceptable if regulated to minimize
 *   pain. Their property status is retained, but constrained by welfare
 *   obligations. This reading attempts to balance human interests with animal
 *   interests, leading to a system of regulated use. It is a 'tangled rope'
 *   because it genuinely coordinates the conflicting demands of industry and
 *   public moral concern, but also extracts suffering from animals and
 *   imposes costs on industry, requiring active enforcement to maintain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status_kernel__welfare_reading, 0.6).
domain_priors:theater_ratio(animal_status_kernel__welfare_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status_kernel__welfare_reading, "Animal Welfare Obligations (Welfare Reading)").
narrative_ontology:topic_domain(animal_status_kernel__welfare_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__welfare_reading, 'da927624-03de-4b60-973c-3b026b519085').
narrative_ontology:cs_kernel_codification('da927624-03de-4b60-973c-3b026b519085', formalized).
narrative_ontology:cs_authority_grounding('da927624-03de-4b60-973c-3b026b519085', lineage).
narrative_ontology:cs_interpretation_layer_present('da927624-03de-4b60-973c-3b026b519085').
narrative_ontology:cs_reading_relation('da927624-03de-4b60-973c-3b026b519085', animal_status_kernel__property_reading, influences).
narrative_ontology:cs_reading_relation('da927624-03de-4b60-973c-3b026b519085', animal_status_kernel__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('da927624-03de-4b60-973c-3b026b519085', foundational, sentience_confers_moral_relevance).
narrative_ontology:cs_axiom_status(sentience_confers_moral_relevance, holdable).
narrative_ontology:cs_axiom_grounding('da927624-03de-4b60-973c-3b026b519085', sentience_confers_moral_relevance, deontological).
narrative_ontology:cs_axiom('da927624-03de-4b60-973c-3b026b519085', foundational, property_status_is_retainable_with_obligations).
narrative_ontology:cs_axiom_status(property_status_is_retainable_with_obligations, holdable).
narrative_ontology:cs_axiom_grounding('da927624-03de-4b60-973c-3b026b519085', property_status_is_retainable_with_obligations, conventional).
narrative_ontology:cs_reference_frame('da927624-03de-4b60-973c-3b026b519085', regulated_use_with_moral_consideration).
narrative_ontology:cs_drift_state('da927624-03de-4b60-973c-3b026b519085', contemporary_animal_rights_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('da927624-03de-4b60-973c-3b026b519085', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__welfare_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, pharmaceutical_research).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, pet_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, consumers).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, farm_animals).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, wild_animals_in_captivity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, consumers).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, sentience_as_moral_criterion).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, utilitarian_ethics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates within welfare regulations, incurring costs for pain minimization but retaining the right to use animals as property. Actively lobbies for less stringent regulations and shapes public discourse on 'humane' practices. Benefits from continued demand for animal products.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_agriculture_industry, agenda_setter,
    institutional, generational, constrained, global).

% Relies on animal testing, subject to welfare regulations that increase costs but permit continued research. Benefits from the ability to use animals for scientific advancement and product safety testing.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, pharmaceutical_research, beneficiary,
    institutional, generational, constrained, global).

% Benefits from the legal status of animals as property, enabling trade and ownership, while adhering to welfare standards for companion animals. Profits from the sale of animals and related products/services.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, pet_industry, beneficiary,
    organized, biographical, mobile, global).

% Benefit from the availability of animal products and services, often at lower prices than if animals were not property. Indirectly bear costs of welfare regulations through higher prices. Their choices are shaped by market availability and ethical considerations.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, consumers, beneficiary,
    moderate, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, consumers, payer).

% Experience suffering within the bounds of welfare regulations, which aim to minimize but not eliminate pain. Their property status means their interests are secondary to human use, despite their sentience. They are born into and die within this system.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, farm_animals, payer,
    powerless, biographical, trapped, global).

% Subjected to scientific procedures, with welfare regulations dictating housing, care, and pain management. Their suffering is deemed acceptable for scientific advancement, and they have no means of escape.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, laboratory_animals, payer,
    powerless, biographical, trapped, global).

% Held in zoos, circuses, or other entertainment venues, their lives are constrained by human purposes. Welfare regulations aim to provide adequate living conditions but do not alter their fundamental captivity or property status.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, wild_animals_in_captivity, payer,
    powerless, biographical, trapped, global).

% Work to improve welfare standards within the existing framework of animal property status. They lobby for stronger regulations and educate the public, but do not challenge the fundamental right to use animals.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_welfare_advocates, observer,
    organized, generational, mobile, global).

% Reject the premise of animal property status and advocate for fundamental rights. They view welfare reforms as perpetuating the system of exploitation rather than alleviating it, and are largely excluded from policy discussions within the welfare framework.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, abolitionist_advocates, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ethical and economic demands of animal use by establishing a framework where animals can be used as property while requiring minimization of their suffering, balancing industry needs with public moral concern.
% TRANSFER_FUNCTION: Transfers the costs of suffering (pain, distress, loss of autonomy) from human users to animals, while transferring the costs of welfare compliance (housing, veterinary care, humane slaughter) from animals to industry, which then passes some to consumers.
% ABSENT_VOICES: Abolitionist advocates are largely absent from the direct policy-making within this framework, as their core premise (animals are not property) is foreclosed by the welfare reading. They would argue that welfare reforms legitimize continued exploitation.
% DISAPPEARANCE_RATIONALE: If the welfare obligations vanished overnight, the animal agriculture, research, and pet industries would immediately revert to practices that prioritize economic efficiency over animal suffering, leading to immense and unregulated animal pain. Public outcry would be significant, and new, likely more extreme, regulations would eventually emerge, but the immediate impact would be a collapse of ethical standards.
% FOUNDING_PROBLEM: The problem of reconciling human desire for animal products and services with growing public awareness of animal sentience and capacity for suffering, without fundamentally altering the economic and legal status of animals as property.
% FOUNDING_PROBLEM_CORROBORATION: Animal welfare organizations and a significant portion of the public attest that the problem of animal suffering in human systems is still live and requires ongoing regulation. Industry stakeholders also acknowledge the need for public trust, which welfare standards help maintain.
narrative_ontology:disappearance_verdict(animal_status_kernel__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__welfare_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_status_kernel__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__welfare_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__welfare_reading_tests).
:- end_tests(animal_status_kernel__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because while welfare regulations impose costs on industry and reduce some forms of suffering, they do not eliminate the fundamental extraction of animal lives and labor. Suppression is moderate (0.6) as animals are trapped within the system, and alternatives to animal use are not actively suppressed but are often economically disadvantaged. Theater ratio is low (0.25) because welfare reforms are often genuine attempts to reduce suffering, though they can also serve to legitimize continued animal use. The metrics show a slight increase in extractiveness and suppression over time, reflecting the ongoing tension between economic pressures and evolving welfare standards.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the industries, this is a necessary regulatory framework that allows for the continued, responsible use of animals. From the perspective of the animals, it is a system that permits their suffering and exploitation, even if minimized. Abolitionists view it as a 'tangled rope' that perpetuates injustice by making exploitation palatable. The engine's classification as 'tangled_rope' reflects this inherent tension and asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The animal agriculture, pharmaceutical, and pet industries, along with consumers, are beneficiaries as they retain the right to use animals, albeit with some regulatory burden. Farm, laboratory, and captive wild animals are the primary payers, bearing the suffering inherent in the system. Animal welfare advocates act as observers, working within the framework, while abolitionist advocates are excluded, as their core premise is incompatible with this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_vs_abolition_efficacy,
    'Does the ''welfare_reading'' genuinely reduce animal suffering, or does it primarily serve to legitimize and perpetuate animal exploitation by making it more palatable to the public (''new welfarism'' critique)?',
    'Empirical studies comparing animal welfare outcomes under different regulatory regimes, and sociological analysis of public perception and consumption patterns in response to welfare reforms.',
    'If welfare reforms are found to primarily legitimize exploitation, the effective extractiveness of this constraint would be higher, and its coordination function would be re-evaluated as more theatrical, potentially shifting its classification closer to a ''snare'' from an abolitionist perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_vs_abolition_efficacy, empirical, 'The true impact of welfare reforms on animal suffering and public acceptance of animal use.').

omega_variable(
    property_status_naturalness,
    'Is the property status of animals a ''natural'' or inevitable consequence of human-animal interaction, or is it a socially constructed legal fiction that could be (and should be) dismantled?',
    'Philosophical and legal analysis of the historical development of animal property law, and cross-cultural comparison of legal systems that grant different statuses to animals.',
    'If property status is found to be a social construct, the ''emerges_naturally'' claim (if present in a different reading) would be undermined, and the ''welfare_reading'' would be seen as a ''tangled rope'' built upon a fundamentally unjust foundation, increasing its perceived extractiveness from a rights-based perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_status_naturalness, conceptual, 'The ontological status of animal property: natural vs. constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__welfare_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1970, animal_status_kernel__welfare_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(anim_tr_t1980, animal_status_kernel__welfare_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(anim_tr_t1990, animal_status_kernel__welfare_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(anim_tr_t2000, animal_status_kernel__welfare_reading, theater_ratio, 2000, 0.23).
narrative_ontology:measurement(anim_tr_t2010, animal_status_kernel__welfare_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(anim_tr_t2024, animal_status_kernel__welfare_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(anim_be_t1970, animal_status_kernel__welfare_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(anim_be_t1980, animal_status_kernel__welfare_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(anim_be_t1990, animal_status_kernel__welfare_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(anim_be_t2000, animal_status_kernel__welfare_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(anim_be_t2010, animal_status_kernel__welfare_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(anim_be_t2024, animal_status_kernel__welfare_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1970, animal_status_kernel__welfare_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(anim_su_t1980, animal_status_kernel__welfare_reading, suppression_requirement, 1980, 0.48).
narrative_ontology:measurement(anim_su_t1990, animal_status_kernel__welfare_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(anim_su_t2000, animal_status_kernel__welfare_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(anim_su_t2010, animal_status_kernel__welfare_reading, suppression_requirement, 2010, 0.59).
narrative_ontology:measurement(anim_su_t2024, animal_status_kernel__welfare_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__welfare_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__welfare_reading, 0.1).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'animal_status_kernel', alongside the 'property_reading' and 'abolitionist_reading'. Each reading instantiates a distinct constraint with different ε values and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
