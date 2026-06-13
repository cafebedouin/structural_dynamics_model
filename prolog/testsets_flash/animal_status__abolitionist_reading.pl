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
 *   constraint_id: animal_status__abolitionist_reading
 *   human_readable: Abolitionist Reading: Animals as Rights-Holders
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint represents the abolitionist reading of animal status,
 *   where animals are considered rights-holders with inherent value, and any
 *   instrumental use is a violation. From this perspective, the current legal
 *   and economic systems that permit animal exploitation constitute a Snare,
 *   extracting maximum value from animals through their legal classification
 *   as property. Welfare reforms are seen as legitimizing the underlying
 *   exploitation rather than alleviating it. This reading is in direct
 *   opposition to the property and welfare readings of the same kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.95).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.99).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.99).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.99).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Abolitionist Reading: Animals as Rights-Holders").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, 'c9bcb562-1fec-4b32-a8b4-bd8fb646d36b').
narrative_ontology:cs_kernel_codification('c9bcb562-1fec-4b32-a8b4-bd8fb646d36b', implicit).
narrative_ontology:cs_authority_grounding('c9bcb562-1fec-4b32-a8b4-bd8fb646d36b', extraction).
narrative_ontology:cs_interpretation_layer_present('c9bcb562-1fec-4b32-a8b4-bd8fb646d36b').
narrative_ontology:cs_reading_relation('c9bcb562-1fec-4b32-a8b4-bd8fb646d36b', animal_status__welfare_reading, forecloses).
narrative_ontology:cs_reading_relation('c9bcb562-1fec-4b32-a8b4-bd8fb646d36b', animal_status__property_reading, forecloses).
narrative_ontology:cs_axiom('c9bcb562-1fec-4b32-a8b4-bd8fb646d36b', foundational, animals_are_rights_holders).
narrative_ontology:cs_axiom_status(animals_are_rights_holders, holdable).
narrative_ontology:cs_axiom_grounding('c9bcb562-1fec-4b32-a8b4-bd8fb646d36b', animals_are_rights_holders, deontological).
narrative_ontology:cs_axiom('c9bcb562-1fec-4b32-a8b4-bd8fb646d36b', foundational, instrumental_use_is_exploitation).
narrative_ontology:cs_axiom_status(instrumental_use_is_exploitation, holdable).
narrative_ontology:cs_axiom_grounding('c9bcb562-1fec-4b32-a8b4-bd8fb646d36b', instrumental_use_is_exploitation, deontological).
narrative_ontology:cs_reference_frame('c9bcb562-1fec-4b32-a8b4-bd8fb646d36b', universal_animal_rights).
narrative_ontology:cs_drift_state('c9bcb562-1fec-4b32-a8b4-bd8fb646d36b', contemporary_legal_status, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('c9bcb562-1fec-4b32-a8b4-bd8fb646d36b', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animals_used_for_food).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animals_used_for_research).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animals_used_for_entertainment).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animals_used_for_clothing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subjected to industrial farming practices, slaughter, and consumption. Their lives are entirely instrumentalized, with no legal standing to resist or exit.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animals_used_for_food, payer,
    powerless, immediate, trapped, global).

% Used in scientific experiments, often involving pain, distress, and death, without consent or independent legal protection. Their existence is solely for human benefit.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animals_used_for_research, payer,
    powerless, immediate, trapped, global).

% Exploited in circuses, zoos, rodeos, and other forms of entertainment, often enduring confinement, forced performance, and unnatural conditions for human amusement.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animals_used_for_entertainment, payer,
    powerless, immediate, trapped, global).

% Bred and killed for their fur, skin, or wool, their bodies treated as commodities for human fashion and utility.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animals_used_for_clothing, payer,
    powerless, immediate, trapped, global).

% Benefits immensely from the instrumental status of animals, driving policy and public perception to maintain the current legal framework that permits widespread animal use. Actively resists any shift towards rights-based status.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_agriculture_industry, agenda_setter,
    institutional, generational, constrained, global).

% Relies on animal models for scientific advancement and product testing. Advocates for the continued legal status of animals as property to facilitate research, while often implementing internal welfare guidelines.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, biomedical_research_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Argue for the full legal personhood and rights of animals, seeking to dismantle all forms of instrumental use. They operate outside the dominant legal and economic frameworks, facing significant resistance.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, abolitionist_advocates, observer,
    organized, generational, constrained, global).

% Seek to improve the conditions of animals within existing instrumental frameworks, advocating for better housing, reduced suffering, and humane slaughter. From an abolitionist perspective, their efforts are seen as legitimizing the underlying exploitation.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, welfare_reformers, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading posits no legitimate coordination function for instrumental animal use, viewing it as pure exploitation. The 'coordination' it observes is the systemic organization of animal exploitation.
% TRANSFER_FUNCTION: Transfers the inherent value, bodily autonomy, and lives of animals to human beings for their consumption, research, entertainment, and material gain.
% ABSENT_VOICES: The animals themselves are the primary absent voices, unable to articulate their interests or consent. Their advocates speak on their behalf, but their direct experience is excluded from legal and ethical consideration.
% DISAPPEARANCE_RATIONALE: If the instrumental status of animals vanished overnight, the global food system, biomedical research, entertainment industries, and fashion sectors would undergo a radical, immediate, and profound reorganization. Billions of animals would cease to be commodities, leading to a complete restructuring of human-animal relations.
% FOUNDING_PROBLEM: The historical problem this constraint (as a Snare) 'solves' is the human desire for cheap animal products and services, by legally defining animals as property without rights, thus externalizing all costs onto them.
% FOUNDING_PROBLEM_CORROBORATION: The problem of human desire for animal products and the economic benefits derived from animal exploitation remains live, as attested by the continued growth of animal agriculture and other animal-use industries. Abolitionist advocates corroborate that the 'problem' is not a genuine coordination challenge but a moral failing perpetuated by the current legal status of animals.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).

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
 *   Extractiveness is near maximal (0.95) because animals are entirely instrumentalized, their lives and bodies treated as commodities. Suppression is also near maximal (0.99) due to their complete lack of legal standing, inability to consent, and the systemic violence inherent in their exploitation. Accessibility collapse is total (0.99) as there are no legal or practical alternatives for animals to exit their status as property. Resistance is minimal (0.01) from the animals themselves, though significant from abolitionist advocates. Theater ratio is very low (0.05) because the system is highly functional in its extractive purpose; any 'welfare' measures are seen as minor adjustments that do not alter the fundamental instrumentalization.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the animal-use industries, animals are property, and their use is a natural and necessary part of human society, with welfare regulations representing a 'rope' of responsible stewardship. From the abolitionist perspective, this is a Snare, a system of total exploitation masked by legal fictions. The engine's classification will reflect the structural reality of extraction and suppression, which aligns with the abolitionist reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals are the full targets (d=1.0) of this constraint, bearing all costs and receiving no benefits. Industries that profit from animal exploitation (animal agriculture, research, entertainment, clothing) are the primary beneficiaries (d=0.0). Abolitionist advocates are observers, seeking to dismantle the constraint. Welfare reformers are seen as inadvertently supporting the constraint by seeking to 'improve' it rather than abolish it, thus legitimizing its existence.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_personhood_feasibility,
    'Is the legal personhood of animals a practically achievable and coherent legal framework, or does it introduce insurmountable complexities for human society?',
    'Development and implementation of legal frameworks for animal personhood in specific jurisdictions, followed by analysis of their practical implications and challenges.',
    'If feasible, it strengthens the abolitionist claim by demonstrating a viable alternative to instrumental use. If not, it highlights the conceptual and practical barriers to full abolition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_personhood_feasibility, empirical, 'Practicality of animal legal personhood.').

omega_variable(
    welfare_vs_abolition_legitimacy,
    'Do animal welfare reforms genuinely reduce suffering, or do they primarily serve to legitimize and perpetuate the underlying system of animal exploitation?',
    'Empirical studies on the long-term impact of welfare reforms on animal suffering and the public''s perception of animal use, analyzed through an abolitionist lens.',
    'If welfare reforms are found to primarily legitimize exploitation, it reinforces the abolitionist critique of the welfare reading. If they significantly reduce suffering without legitimizing, it challenges the abolitionist rejection of welfare as a stepping stone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_vs_abolition_legitimacy, conceptual, 'Impact of welfare reforms on exploitation legitimacy.').

omega_variable(
    natural_vs_constructed_speciesism,
    'Is speciesism an inherent, unavoidable aspect of human nature and interspecies relations, or is it a socially constructed ideology that can be overcome?',
    'Cross-cultural and historical analysis of human-animal relations, alongside psychological and sociological studies on the origins and persistence of speciesist attitudes.',
    'If inherent, the abolitionist project faces a fundamental ''mountain'' of human nature. If constructed, it is a ''snare'' of ideology that can be dismantled through social and political change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_speciesism, empirical, 'Inherent vs. constructed nature of speciesism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 1600, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1600, animal_status__abolitionist_reading, theater_ratio, 1600, 0.01).
narrative_ontology:measurement(anim_tr_t1700, animal_status__abolitionist_reading, theater_ratio, 1700, 0.01).
narrative_ontology:measurement(anim_tr_t1800, animal_status__abolitionist_reading, theater_ratio, 1800, 0.02).
narrative_ontology:measurement(anim_tr_t1900, animal_status__abolitionist_reading, theater_ratio, 1900, 0.03).
narrative_ontology:measurement(anim_tr_t2000, animal_status__abolitionist_reading, theater_ratio, 2000, 0.04).
narrative_ontology:measurement(anim_tr_t2024, animal_status__abolitionist_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(anim_be_t1600, animal_status__abolitionist_reading, base_extractiveness, 1600, 0.9).
narrative_ontology:measurement(anim_be_t1700, animal_status__abolitionist_reading, base_extractiveness, 1700, 0.92).
narrative_ontology:measurement(anim_be_t1800, animal_status__abolitionist_reading, base_extractiveness, 1800, 0.93).
narrative_ontology:measurement(anim_be_t1900, animal_status__abolitionist_reading, base_extractiveness, 1900, 0.94).
narrative_ontology:measurement(anim_be_t2000, animal_status__abolitionist_reading, base_extractiveness, 2000, 0.95).
narrative_ontology:measurement(anim_be_t2024, animal_status__abolitionist_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1600, animal_status__abolitionist_reading, suppression_requirement, 1600, 0.9).
narrative_ontology:measurement(anim_su_t1700, animal_status__abolitionist_reading, suppression_requirement, 1700, 0.92).
narrative_ontology:measurement(anim_su_t1800, animal_status__abolitionist_reading, suppression_requirement, 1800, 0.94).
narrative_ontology:measurement(anim_su_t1900, animal_status__abolitionist_reading, suppression_requirement, 1900, 0.96).
narrative_ontology:measurement(anim_su_t2000, animal_status__abolitionist_reading, suppression_requirement, 2000, 0.98).
narrative_ontology:measurement(anim_su_t2024, animal_status__abolitionist_reading, suppression_requirement, 2024, 0.99).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
