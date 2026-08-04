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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Abolitionist Animal Rights: Animals as Rights-Holders
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint represents the abolitionist reading of animal status,
 *   asserting that animals are rights-holders with inherent value, precluding
 *   all instrumental use. It is a 'snare' from the perspective of animals,
 *   who are trapped in systems of exploitation, and from the perspective of
 *   abolitionist advocates, who face immense structural suppression. The
 *   constraint's persistence relies on the active suppression of animal
 *   agency and the legal fiction of animal property. Welfare reforms are seen
 *   as theatrical, merely legitimizing the underlying extraction.
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
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Abolitionist Animal Rights: Animals as Rights-Holders").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, 'ae0a6f97-e935-4721-8e7a-ad581f36012a').
narrative_ontology:cs_kernel_codification('ae0a6f97-e935-4721-8e7a-ad581f36012a', distributed).
narrative_ontology:cs_authority_grounding('ae0a6f97-e935-4721-8e7a-ad581f36012a', diffuse_epistemic).
narrative_ontology:cs_reading_relation('ae0a6f97-e935-4721-8e7a-ad581f36012a', animal_status__welfare_reading, forecloses).
narrative_ontology:cs_reading_relation('ae0a6f97-e935-4721-8e7a-ad581f36012a', animal_status__property_reading, forecloses).
narrative_ontology:cs_axiom('ae0a6f97-e935-4721-8e7a-ad581f36012a', foundational, animals_are_persons_not_property).
narrative_ontology:cs_axiom_status(animals_are_persons_not_property, holdable).
narrative_ontology:cs_axiom_grounding('ae0a6f97-e935-4721-8e7a-ad581f36012a', animals_are_persons_not_property, deontological).
narrative_ontology:cs_axiom('ae0a6f97-e935-4721-8e7a-ad581f36012a', foundational, speciesism_is_unjust_discrimination).
narrative_ontology:cs_axiom_status(speciesism_is_unjust_discrimination, holdable).
narrative_ontology:cs_axiom_grounding('ae0a6f97-e935-4721-8e7a-ad581f36012a', speciesism_is_unjust_discrimination, deontological).
narrative_ontology:cs_reference_frame('ae0a6f97-e935-4721-8e7a-ad581f36012a', universal_moral_consideration).
narrative_ontology:cs_drift_state('ae0a6f97-e935-4721-8e7a-ad581f36012a', contemporary_legal_frameworks, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('ae0a6f97-e935-4721-8e7a-ad581f36012a', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animals_in_instrumental_use).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, biomedical_researchers).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, inherent_value_of_sentient_life).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, species_equality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Animals used in agriculture, research, entertainment, and other industries. They are the direct targets of instrumental use, experiencing suffering and death, with no legal standing or means of exit from their status as property.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animals_in_instrumental_use, payer,
    powerless, immediate, trapped, universal).

% Advocate for the full legal and moral recognition of animals as rights-holders, seeking to dismantle all systems of instrumental use. They engage in public education, protest, and legal challenges, often facing significant opposition.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, abolitionist_advocates, agenda_setter,
    organized, generational, constrained, global).

% Seek to improve the conditions of animals within existing systems of use, advocating for better housing, reduced suffering, and humane slaughter. From the abolitionist perspective, their efforts are seen as legitimizing exploitation rather than ending it.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_welfare_reformers, excluded,
    organized, biographical, mobile, national).

% Benefits from the current legal status of animals as property, allowing for their instrumental use in food production. This industry actively resists any changes that would elevate animal status to rights-holders, as it would dismantle their business model.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_agriculture_industry, beneficiary,
    institutional, biographical, arbitrage, global).

% Rely on the instrumental use of animals for scientific experimentation and drug development. They argue for the necessity of animal models for human health advancements and resist abolitionist claims.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, biomedical_researchers, beneficiary,
    institutional, biographical, constrained, global).

% Analyze the conceptual foundations of animal rights, personhood, and legal standing. They critically examine the arguments for and against instrumental use, contributing to the intellectual discourse without direct enforcement power.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, legal_philosophers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This constraint, if fully implemented, would coordinate human behavior around a universal recognition of animal rights, ending speciesist discrimination and aligning human actions with the inherent value of all sentient life.
% TRANSFER_FUNCTION: It would transfer the 'right to use' animals from humans to animals themselves, effectively ending the transfer of animal bodies, labor, and products for human benefit. This would entail a massive economic and social restructuring.
% ABSENT_VOICES: Future generations of animals, who would benefit from a world free of instrumental use, are absent. Also, non-human animals themselves, whose interests are represented by advocates but who cannot directly articulate their claims in human legal systems.
% DISAPPEARANCE_RATIONALE: If the abolitionist reading of animal rights were universally adopted overnight, the global economy, legal systems, and human-animal relationships would undergo a profound and immediate rearrangement. Industries reliant on animal exploitation would collapse, and new ethical frameworks for human interaction with the natural world would emerge.
% FOUNDING_PROBLEM: The historical and ongoing instrumentalization of animals by humans, leading to immense suffering, environmental degradation, and a moral inconsistency in human ethics (speciesism).
% FOUNDING_PROBLEM_CORROBORATION: The problem of animal suffering and exploitation is widely documented by scientific research, investigative journalism, and animal advocacy organizations. While the 'solution' (abolition) is contested, the existence of the problem is corroborated by extensive evidence from outside the benefiting industries.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
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

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) because animals are treated as mere resources, with their lives and bodies fully appropriated for human benefit. Suppression is also very high (0.98) due to the legal and economic structures that enforce animal property status and actively prevent any meaningful resistance or exit for animals. Theater ratio is low (0.05) because the constraint is brutally functional in its extraction; welfare measures, while present, are seen by this reading as minor adjustments that do not alter the fundamental extractive nature. Accessibility collapse is low (0.1) because the abolitionist reading actively seeks to create alternatives to instrumental use, but resistance is high (0.85) due to the ongoing efforts of abolitionist movements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of animals, this constraint is a pure snare, trapping them in a system of total extraction. From the perspective of abolitionist advocates, it is a snare that they are actively fighting to dismantle. From the perspective of industries that use animals, the current status is a 'rope' that coordinates resource allocation, or even a 'mountain' reflecting natural human dominance. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals are the ultimate targets (d=1.0), bearing the full cost of instrumental use. Abolitionist advocates are also targets (d high) as they bear the costs of challenging a deeply entrenched system. Industries that use animals are beneficiaries (d=0.0), profiting directly from the current legal and ethical framework. Welfare reformers are seen as indirectly benefiting the extractive system by legitimizing it, or as constrained payers if their reforms impose costs without ending exploitation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to allow instrumental use of animals for human benefit) is still 'live' from the perspective of benefiting industries, but 'dead' or 'contested' from the abolitionist perspective, which views it as an outdated moral framework. The high extractiveness and suppression, coupled with the 'live' founding problem status (from the perspective of those who benefit from the status quo), indicate a snare where the original 'coordination' (human mastery over nature) has become pure extraction. The abolitionist reading rejects any notion of a coordination function in instrumental use.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the instrumental use of animals a ''natural'' outcome of human-animal relations, or a social, legal, and economic construct?',
    'Cross-cultural and historical analysis of human-animal relationships, examining societies where instrumental use is minimal or absent, and philosophical arguments regarding the basis of moral status.',
    'If a natural law, the constraint is closer to a mountain, making abolition impossible. If a social construct, it is a snare, making abolition a matter of political will and moral evolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Whether instrumental animal use is natural or constructed.').

omega_variable(
    welfare_reform_legitimation,
    'Do animal welfare reforms genuinely reduce suffering and move towards abolition, or do they primarily serve to legitimize and perpetuate instrumental use by making it appear more ''humane''?',
    'Empirical studies on the long-term effects of welfare reforms on animal suffering and the public perception of animal exploitation, alongside critical analysis of the philosophical underpinnings of welfare vs. rights.',
    'If reforms primarily legitimize, the ''theater_ratio'' for the overall system of animal use is higher than currently measured, and the ''snare'' classification is reinforced. If they genuinely reduce suffering and pave the way for rights, the system might be a ''tangled_rope'' with a path to ''rope'' or ''scaffold''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_legitimation, empirical, 'Impact of welfare reforms on the legitimacy of animal exploitation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of animal agency structural (legal property status, physical confinement) or internalized (lack of cognitive capacity for collective resistance, species-specific limitations)?',
    'Comparative ethological studies on animal communication and social organization, combined with legal analysis of how property law actively prevents animal self-determination. If suppression persists after legal barriers are removed, it is partially internalized.',
    'If primarily structural, the constraint''s effective suppression is directly tied to human-designed systems. If significantly internalized, the challenge of ''liberation'' becomes more complex, involving species-specific needs and capacities beyond legal reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for animals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1970, animal_status__abolitionist_reading, theater_ratio, 1970, 0.01).
narrative_ontology:measurement(anim_tr_t1980, animal_status__abolitionist_reading, theater_ratio, 1980, 0.02).
narrative_ontology:measurement(anim_tr_t1990, animal_status__abolitionist_reading, theater_ratio, 1990, 0.03).
narrative_ontology:measurement(anim_tr_t2000, animal_status__abolitionist_reading, theater_ratio, 2000, 0.04).
narrative_ontology:measurement(anim_tr_t2010, animal_status__abolitionist_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(anim_tr_t2024, animal_status__abolitionist_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(anim_be_t1970, animal_status__abolitionist_reading, base_extractiveness, 1970, 0.99).
narrative_ontology:measurement(anim_be_t1980, animal_status__abolitionist_reading, base_extractiveness, 1980, 0.98).
narrative_ontology:measurement(anim_be_t1990, animal_status__abolitionist_reading, base_extractiveness, 1990, 0.97).
narrative_ontology:measurement(anim_be_t2000, animal_status__abolitionist_reading, base_extractiveness, 2000, 0.96).
narrative_ontology:measurement(anim_be_t2010, animal_status__abolitionist_reading, base_extractiveness, 2010, 0.95).
narrative_ontology:measurement(anim_be_t2024, animal_status__abolitionist_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1970, animal_status__abolitionist_reading, suppression_requirement, 1970, 0.99).
narrative_ontology:measurement(anim_su_t1980, animal_status__abolitionist_reading, suppression_requirement, 1980, 0.99).
narrative_ontology:measurement(anim_su_t1990, animal_status__abolitionist_reading, suppression_requirement, 1990, 0.98).
narrative_ontology:measurement(anim_su_t2000, animal_status__abolitionist_reading, suppression_requirement, 2000, 0.98).
narrative_ontology:measurement(anim_su_t2010, animal_status__abolitionist_reading, suppression_requirement, 2010, 0.98).
narrative_ontology:measurement(anim_su_t2024, animal_status__abolitionist_reading, suppression_requirement, 2024, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, identity_coordination).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, meat_consumption_norms).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, biomedical_research_ethics).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'animal_status' kernel, focusing on the abolitionist perspective. It directly challenges the 'property_reading' and rejects the 'welfare_reading' as insufficient.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
