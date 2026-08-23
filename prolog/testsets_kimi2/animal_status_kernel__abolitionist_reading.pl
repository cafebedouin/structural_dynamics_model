% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__abolitionist_reading, []).

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
 *   constraint_id: animal_status_kernel__abolitionist_reading
 *   human_readable: Animal Property Status â Abolitionist Reading
 *   domain: moral_legal/animal_ethics
 *
 * SUMMARY:
 *   This constraint story captures the abolitionist reading of the
 *   animal_status_kernel: the standing legal and social arrangement that
 *   classifies nonhuman animals as human property. From this reading, the
 *   constraint is a tangled rope â it genuinely coordinates a massive
 *   sphere of human economic activity (agriculture, research, companionship,
 *   entertainment) while asymmetrically extracting the bodies, labor, and
 *   lives of nonhuman animals. The property framework is enforced by state
 *   legal apparatuses worldwide, requires active maintenance against animal
 *   personhood claims, and generates substantial concentrated benefits for
 *   animal-using industries and diffuse benefits for human consumers. The
 *   abolitionist reading holds that this arrangement is not merely in need of
 *   welfare reform but is fundamentally unjust because it treats moral
 *   persons as property. This is one reading of a contested kernel; sibling
 *   readings include the property reading (animals as pure property) and the
 *   welfare reading (property retained but regulated).
 *
 * KEY AGENTS:
 *   - nonhuman_animals: Primary target (powerless/trapped) â bears total extraction of bodily autonomy, reproductive capacity, and life.
 *   - animal_exploitation_industries: Primary beneficiary (institutional/arbitrage) â collects revenue from animal bodies and labor.
 *   - human_consumers: Secondary beneficiary (moderate/mobile) â receives subsidized access to animal products and labor.
 *   - state_legal_apparatus: Agenda-setter (institutional/analytical) â administers and enforces property classification.
 *   - abolitionist_advocates: Analytical observer (organized/analytical) â sees the full structure and argues for abolition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, 0.94).
domain_priors:suppression_score(animal_status_kernel__abolitionist_reading, 0.82).
domain_priors:theater_ratio(animal_status_kernel__abolitionist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, extractiveness, 0.94).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__abolitionist_reading, tangled_rope).
narrative_ontology:human_readable(animal_status_kernel__abolitionist_reading, "Animal Property Status â Abolitionist Reading").
narrative_ontology:topic_domain(animal_status_kernel__abolitionist_reading, "moral_legal/animal_ethics").

domain_priors:requires_active_enforcement(animal_status_kernel__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__abolitionist_reading, '04b04752-a74e-48fb-b4bf-43dc454bed95').
narrative_ontology:cs_kernel_codification('04b04752-a74e-48fb-b4bf-43dc454bed95', formalized).
narrative_ontology:cs_authority_grounding('04b04752-a74e-48fb-b4bf-43dc454bed95', lineage).
narrative_ontology:cs_interpretation_layer_present('04b04752-a74e-48fb-b4bf-43dc454bed95').
narrative_ontology:cs_reading_relation('04b04752-a74e-48fb-b4bf-43dc454bed95', animal_status_kernel__property_reading, forecloses).
narrative_ontology:cs_reading_relation('04b04752-a74e-48fb-b4bf-43dc454bed95', animal_status_kernel__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('04b04752-a74e-48fb-b4bf-43dc454bed95', foundational, animals_are_moral_persons).
narrative_ontology:cs_axiom_status(animals_are_moral_persons, holdable).
narrative_ontology:cs_axiom_grounding('04b04752-a74e-48fb-b4bf-43dc454bed95', animals_are_moral_persons, deontological).
narrative_ontology:cs_axiom('04b04752-a74e-48fb-b4bf-43dc454bed95', foundational, property_status_inherently_unjust).
narrative_ontology:cs_axiom_status(property_status_inherently_unjust, holdable).
narrative_ontology:cs_axiom_grounding('04b04752-a74e-48fb-b4bf-43dc454bed95', property_status_inherently_unjust, deontological).
narrative_ontology:cs_reference_frame('04b04752-a74e-48fb-b4bf-43dc454bed95', legal_property_framework).
narrative_ontology:cs_drift_state('04b04752-a74e-48fb-b4bf-43dc454bed95', contemporary_welfare_regulation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('04b04752-a74e-48fb-b4bf-43dc454bed95', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__abolitionist_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, animal_exploitation_industries).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, human_consumers).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, nonhuman_animals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classified as legal property worldwide; subject to ownership, breeding, confinement, and killing at human discretion. Cannot contest status, vote, or opt out of the property relation. Bodily autonomy, reproductive capacity, and life are extracted continuously.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, nonhuman_animals, payer,
    powerless, immediate, trapped, global).

% Own and trade nonhuman animals as capital assets across agriculture, pharmaceutical testing, entertainment, and textile supply chains. Collect revenue from animal bodies, secretions, and labor. Lobby aggressively to maintain property classification and resist personhood claims.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_exploitation_industries, beneficiary,
    institutional, generational, arbitrage, global).

% Access animal-derived food, materials, and entertainment at prices subsidized by externalizing costs onto animals. Face cultural and infrastructural friction when attempting plant-based alternatives, but retain individual mobility to exit consumption.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, human_consumers, beneficiary,
    moderate, biographical, mobile, global).

% Codifies and enforces the property classification of animals through statutory law, case law, and policing. Adjudicates ownership disputes and criminalizes interference with animal property. Could abolish the category through legislative or constitutional action but maintains it as a foundational legal default.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Analyze and publicly contest the property framework, arguing for legal personhood and rights-based abolition. Hold analytical distance from the constraint's benefit/transfer flows but lack institutional power to directly alter property law.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, abolitionist_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__abolitionist_reading, animal_exploitation_industries).
narrative_ontology:fixing_cost_class(animal_status_kernel__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates ownership, control, and tradability of nonhuman animals among human legal persons, creating a standardized asset class that coordinates breeding, labor deployment, and killing across global supply chains.
% TRANSFER_FUNCTION: Transfers the bodies, reproductive labor, lives, and metabolic products of nonhuman animals to human owners and consumers; transfers wealth generated by animal commodification to exploitation industries and downstream consumers via suppressed pricing.
% ABSENT_VOICES: Nonhuman animals are structurally excluded from legal standing and political voice; they would object to their property status if they could participate in the normative conversation. Abolitionist advocates are present in discourse but excluded from institutional agenda-setting power.
% DISAPPEARANCE_RATIONALE: If animal property status vanished overnight, global agriculture, pharmaceutical testing, food systems, fashion, and entertainment industries would collapse or require fundamental reorganization. Legal frameworks would need rewriting, supply chains would halt, and human-animal relations would shift from ownership to guardianship or autonomous coexistence.
% FOUNDING_PROBLEM: Coordination of human use of nonhuman animals and resolution of disputes over who may kill, confine, breed, or profit from them; creation of a stable legal category for tradable living capital.
% FOUNDING_PROBLEM_CORROBORATION: Animal industry representatives attest the problem is live, citing ongoing demand for animal products. Abolitionist ethicists and plant-based systems researchers attest the problem is dead because nutritional and material alternatives have removed the need. Independent agricultural economists and public health scientists outside both camps provide mixed corroboration: they confirm alternatives are viable at scale but note infrastructural lock-in and cultural persistence.
narrative_ontology:disappearance_verdict(animal_status_kernel__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status_kernel__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__abolitionist_reading, 0.94, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-maximum (0.94) because the property framework enables total extraction: animals are bred, owned, and killed at human discretion. Suppression is high (0.82) because the arrangement depends on actively excluding animal personhood from legal standing, criminalizing direct action on behalf of animals (ag-gag laws, property crime designation), and maintaining a cultural ontology that renders animal subjectivity invisible. Theater ratio is substantial (0.58) and rising: welfare regulations and 'humane' labeling perform concern while preserving the property relation. Accessibility collapse is high (0.78) because once the property framework is accepted as natural, abolition appears utopian and exit (veganism) appears as a personal lifestyle choice rather than a political response to injustice. Resistance is moderate (0.55): a global animal rights movement exists but remains politically marginal relative to the institutional power of animal industries.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (industries, consumers) experience the constraint as coordinating legitimate economic activity and satisfying demand. The payer seat (nonhuman_animals) experiences total domination. The agenda-setter seat (state) experiences a routine administrative function. The analytical observer seat (abolitionists) experiences a fundamental rights violation. These divergences are structural: they follow from power, exit options, and position in the property relation, not from mere disagreement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (animal_exploitation_industries, human_consumers) receive low directionality: the constraint subsidizes their access to animal bodies and labor. The payer (nonhuman_animals) receives maximum directionality: the constraint extracts everything from them. The agenda_setter (state_legal_apparatus) sits near symmetric: it enforces but does not directly capture the extraction. The observer (abolitionist_advocates) is analytical and outside the benefit/transfer flow.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this constraint as tangled rope rather than snare preserves the genuine coordination function of property law (dispute resolution, transfer protocols, economic planning) while registering the asymmetric extraction that rides on it. A snare classification would incorrectly claim the coordination is pure cover; a rope classification would erase the animal victim-set. The tangled rope classification captures that property law does real coordinative work for humans and simultaneously does real extractive work to animals. Mandatrophy would occur if welfare reforms were mistaken for the abolition of extraction â the sunset of welfare theater would be misread as the sunset of the constraint itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_reform_strategic_effect,
    'Do welfare reforms for animals structurally delay abolition by legitimizing property status, or advance it by shifting moral thresholds and building institutional capacity for regulation?',
    'Historical comparative analysis of jurisdictions with strong welfare regulation versus none, tracking time-to-abolition proxies, public attitude shifts, and industry consolidation patterns.',
    'If welfare delays abolition, the abolitionist reading is strengthened and the welfare reading may be reclassified as extraction-dampening theater. If welfare advances abolition, the abolitionist reading must accommodate tactical welfare support without abandoning categorical principles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_strategic_effect, empirical, 'Empirical dispute over whether welfare reforms delay or advance abolition.').

omega_variable(
    personhood_scope_boundary,
    'Does the abolitionist personhood claim extend to all animals, only sentient animals, or some other boundary, and how does this boundary affect the constraint''s victim-set and extraction metrics?',
    'Philosophical analysis of cognitive criteria combined with neuroscientific consensus on sentience; legal tests for personhood capacity.',
    'A narrow boundary (sentience-only) would reduce the victim-set and lower extractiveness for non-sentient animals; a universal boundary would maintain the full victim-set but encounter greater resistance and accessibility-collapse challenges.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(personhood_scope_boundary, conceptual, 'Scope boundary of moral personhood under abolitionist framework.').

omega_variable(
    kernel_reading_alternatives,
    'This constraint is the abolitionist reading of the animal_status_kernel. The welfare reading would retain property status with regulatory constraints, substantially lowering extractiveness and reclassifying the constraint. Which structural elements of the property system are reading-independent and which are reading-indexed?',
    'Comparative analysis of the three readings'' epsilon referents; identification of which facts about animal property law all readings accept and which are interpretation-dependent.',
    'If the core property structure is reading-independent, the kernel is a genuine commitment system with divergent interpretations. If the structure itself shifts across readings, the constraints are distinct arrangements rather than readings of one kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternatives, conceptual, 'Structural decomposition of reading-independent versus reading-indexed elements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__abolitionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_status_abolitionist_tr_t0, animal_status_kernel__abolitionist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(animal_status_abolitionist_tr_t10, animal_status_kernel__abolitionist_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(animal_status_abolitionist_tr_t20, animal_status_kernel__abolitionist_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(animal_status_abolitionist_tr_t30, animal_status_kernel__abolitionist_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement(animal_status_abolitionist_tr_t40, animal_status_kernel__abolitionist_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement(animal_status_abolitionist_tr_t50, animal_status_kernel__abolitionist_reading, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(animal_status_abolitionist_be_t0, animal_status_kernel__abolitionist_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(animal_status_abolitionist_be_t10, animal_status_kernel__abolitionist_reading, base_extractiveness, 10, 0.87).
narrative_ontology:measurement(animal_status_abolitionist_be_t20, animal_status_kernel__abolitionist_reading, base_extractiveness, 20, 0.89).
narrative_ontology:measurement(animal_status_abolitionist_be_t30, animal_status_kernel__abolitionist_reading, base_extractiveness, 30, 0.91).
narrative_ontology:measurement(animal_status_abolitionist_be_t40, animal_status_kernel__abolitionist_reading, base_extractiveness, 40, 0.92).
narrative_ontology:measurement(animal_status_abolitionist_be_t50, animal_status_kernel__abolitionist_reading, base_extractiveness, 50, 0.94).

% Suppression requirement over time
narrative_ontology:measurement(animal_status_abolitionist_su_t0, animal_status_kernel__abolitionist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(animal_status_abolitionist_su_t10, animal_status_kernel__abolitionist_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(animal_status_abolitionist_su_t20, animal_status_kernel__abolitionist_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(animal_status_abolitionist_su_t30, animal_status_kernel__abolitionist_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(animal_status_abolitionist_su_t40, animal_status_kernel__abolitionist_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement(animal_status_abolitionist_su_t50, animal_status_kernel__abolitionist_reading, suppression_requirement, 50, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__abolitionist_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__welfare_reading).

% DUAL FORMULATION NOTE:
% The animal_status_kernel decomposes into three readings (abolitionist, property, welfare) that share the referent of animal legal status but author different epsilon values and victim/beneficiary structures. This story is the abolitionist reading; siblings instantiate the property and welfare readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
