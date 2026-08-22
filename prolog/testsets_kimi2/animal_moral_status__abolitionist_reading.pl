% ============================================================================
% CONSTRAINT STORY: animal_moral_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__abolitionist_reading, []).

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
 *   constraint_id: animal_moral_status__abolitionist_reading
 *   human_readable: Animal Property Status (Abolitionist Reading)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the abolitionist reading of the
 *   animal_moral_status kernel: animals are rights-bearing individuals, legal
 *   property status itself is the violation, and all useâhowever
 *   'humane'âperpetuates victimization. The standing arrangement under
 *   contest is the global legal and economic system that classifies animals
 *   as property. Sibling readings include the property_reading (animals as
 *   resources with no independent moral standing) and the welfare_reading
 *   (regulated use with minimized suffering). This reading carries the
 *   highest Îµ in the kernel family because it recognizes no legitimate
 *   coordination function and no legitimate beneficiary class; the entire
 *   property relation is read as extraction.
 *
 * KEY AGENTS:
 *   - animals_under_human_dominion (payer, powerless, trapped) â bears total extraction of life and liberty under property law
 *   - animal_use_industries (agenda_setter, powerful, mobile) â administers extraction and lobbies to maintain property status
 *   - state_legal_system (agenda_setter, institutional, analytical) â codifies and enforces the property framework
 *   - animal_rights_advocates (observer, organized, analytical) â challenges the constraint from outside the benefiting structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, 0.88).
domain_priors:suppression_score(animal_moral_status__abolitionist_reading, 0.82).
domain_priors:theater_ratio(animal_moral_status__abolitionist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_moral_status__abolitionist_reading, "Animal Property Status (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_moral_status__abolitionist_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__abolitionist_reading, '69961f14-d702-4c6d-9070-a30fbda0bb78').
narrative_ontology:cs_kernel_codification('69961f14-d702-4c6d-9070-a30fbda0bb78', formalized).
narrative_ontology:cs_authority_grounding('69961f14-d702-4c6d-9070-a30fbda0bb78', lineage).
narrative_ontology:cs_interpretation_layer_present('69961f14-d702-4c6d-9070-a30fbda0bb78').
narrative_ontology:cs_reading_relation('69961f14-d702-4c6d-9070-a30fbda0bb78', animal_moral_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('69961f14-d702-4c6d-9070-a30fbda0bb78', animal_moral_status__welfare_reading, forecloses).
narrative_ontology:cs_axiom('69961f14-d702-4c6d-9070-a30fbda0bb78', foundational, animals_are_rights_bearing_individuals).
narrative_ontology:cs_axiom_status(animals_are_rights_bearing_individuals, holdable).
narrative_ontology:cs_axiom_grounding('69961f14-d702-4c6d-9070-a30fbda0bb78', animals_are_rights_bearing_individuals, deontological).
narrative_ontology:cs_axiom('69961f14-d702-4c6d-9070-a30fbda0bb78', foundational, property_status_is_inherent_violation).
narrative_ontology:cs_axiom_status(property_status_is_inherent_violation, holdable).
narrative_ontology:cs_axiom_grounding('69961f14-d702-4c6d-9070-a30fbda0bb78', property_status_is_inherent_violation, deontological).
narrative_ontology:cs_reference_frame('69961f14-d702-4c6d-9070-a30fbda0bb78', nonhuman_personhood_framework).
narrative_ontology:cs_drift_state('69961f14-d702-4c6d-9070-a30fbda0bb78', contemporary_industrial_animal_use, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('69961f14-d702-4c6d-9070-a30fbda0bb78', '').
narrative_ontology:cs_kernel_id(animal_moral_status__abolitionist_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, animals_under_human_dominion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legally classified as property across all jurisdictions. Subjected to ownership, confinement, instrumental use, and killing in food, research, entertainment, and fiber systems. Cannot exit the property relation; their interests are legally subordinate to human owner preferences.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animals_under_human_dominion, payer,
    powerless, biographical, trapped, global).

% Corporations and operations that own, breed, process, and sell animal bodies and products. Exercise substantial lobbying power to maintain property-status laws and resist personhood litigation. Administer the day-to-day extraction and capture the economic gains of the property system.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_use_industries, agenda_setter,
    powerful, generational, mobile, global).

% Codifies animals as property in statutory and common law; adjudicates ownership disputes and criminalizes property-damage to animals while permitting owner-inflicted harm. Provides the enforceable legal architecture that makes non-property status for animals structurally unavailable.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, state_legal_system, agenda_setter,
    institutional, civilizational, analytical, global).

% Challenge animal property status through personhood litigation, direct rescue, and philosophical argument. Seek abolition of animal use rather than welfare reform. Occupy an analytical and oppositional seat relative to the property system.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates animal bodies and labor across human economic sectors; provides legal predictability for ownership and transfer of animal resources; resolves competing human claims to animal use.
% TRANSFER_FUNCTION: Moves animal bodies, reproductive capacity, labor, and products from animals to human owners and industries; transfers legal immunity for use into human hands.
% ABSENT_VOICES: Animals themselves are structurally excluded from legal standing and political voice; future generations who might inherit a non-extractive relationship with animals are not represented; indigenous cosmologies that reject animal property are marginalized in legal frameworks.
% DISAPPEARANCE_RATIONALE: If animal property status vanished, global agricultural, research, and entertainment systems would face immediate legal and economic collapse; supply chains would reorganize around non-animal alternatives or guardianship models; the legal category of livestock would dissolve.
% FOUNDING_PROBLEM: How to establish predictable human ownership and control over animal bodies for food, labor, and materials in settled agricultural societies; how to resolve competing human claims to animals.
% FOUNDING_PROBLEM_CORROBORATION: Animal rights scholars, food systems scientists, and materials researchers outside the animal-use industries attest that the material necessity of animal use has dissolved with plant-based and cellular alternatives; the animal-use industries assert the problem remains live. Independent academic corroboration from non-benefiting seats supports the dead status.
narrative_ontology:disappearance_verdict(animal_moral_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__abolitionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_moral_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__abolitionist_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_moral_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because property status extracts the entire being of animalsâtheir lives, bodies, reproductive systems, and laborâtransferring them wholesale to human owners. Suppression is high (0.82) because the constraint persists through active legal coercion: ag-gag laws, criminalization of rescue, exclusion of animals from legal standing, and marginalization of non-extractive economies. Theater_ratio is moderate-high (0.45) because welfare regulations and 'humane' labeling perform compassion while leaving the property kernel intact, making the constraint appear softer than it is. Accessibility_collapse is substantial (0.68) because once property status is accepted as normal, non-exploitative alternatives (vegan food systems, nonhuman personhood) appear fringe or impossible. Resistance is moderate (0.60) because an organized animal rights movement exists but remains institutionally outmatched.
 *
 * PERSPECTIVAL GAP:
 *   The animal_use_industries experience the property system as necessary economic infrastructure; the state experiences it as settled law. The animals experience it as total, unremitting extraction. The observer seat (advocates) sees the structure as an illegitimate snare. The engine computes these divergences from the structural data rather than adjudicating them.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries are declared in base_properties, consistent with the abolitionist structural delta that recognizes no legitimate beneficiary of animal use. Victims are declared as animals_under_human_dominion, driving their directionality toward the full-target end (d near 1.0). Animal_use_industries and state_legal_system are authored as agenda_setters without beneficiary declarations; their directionality falls to the canonical fallback for powerful/institutional atoms, but their structural role is to administer and enforce extraction. Animal_rights_advocates occupy an observer seat with analytical exit. The engine will compute the largest per-seat divergence between the trapped, powerless payer and the mobile, powerful agenda_setters.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâestablishing predictable control over animal bodies in pre-industrial societiesâis dead: alternatives now exist for food, materials, and research. However, the constraint persists not by inertia alone but by active enforcement and economic capture, so it remains a live snare rather than an atrophied piton. Were enforcement to decay without institutional replacement, the property system might degrade toward piton status; currently it does not meet the piton test because extraction is concentrated and actively maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contested_kernel_reading,
    'This constraint is the abolitionist reading of the animal_moral_status kernel. How does the structural classification change if the welfare reading (regulated use) or property reading (no moral standing) is adopted instead?',
    'Cross-reading comparison: the welfare reading would introduce beneficiaries (humane-certified industries) and reduce the victim set to cruelty cases; the property reading would eliminate victims entirely and reduce extractiveness to near-zero (from that seat, no extraction occurs).',
    'If the welfare reading is adopted, the constraint reclassifies as tangled_rope with moderate extraction; if the property reading is adopted, it reclassifies as rope or mountain (from that seat, the arrangement is natural coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contested_kernel_reading, conceptual, 'Reading-indexed classification variance across the animal_moral_status kernel').

omega_variable(
    property_status_contingency,
    'Is animal property status a contingent legal construction or a necessary structural feature of human civilization?',
    'Comparative legal history and institutional possibility: examining jurisdictions that have granted nonhuman personhood and tracking whether industrial economies can function without animal property.',
    'If contingent, snare classification holds; if necessary or naturalized, reclassification toward mountain or tangled_rope with high extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_status_contingency, conceptual, 'Contingency of animal property status').

omega_variable(
    welfare_reform_function,
    'Do animal welfare reforms function as genuine harm reduction or as theatrical reinforcement of the property kernel?',
    'Temporal correlation analysis between welfare reform implementation and total animal use volumes; if welfare reforms correlate with expanded use, they function as theater.',
    'If welfare is theater, theater_ratio and suppression are higher than measured; if genuine harm reduction, the constraint may have scaffold properties with a sunset path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_function, empirical, 'Welfare reform as theater or genuine harm reduction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__abolitionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__abolitionist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(anim_tr_t10, animal_moral_status__abolitionist_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(anim_tr_t20, animal_moral_status__abolitionist_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(anim_tr_t30, animal_moral_status__abolitionist_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__abolitionist_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement(anim_tr_t50, animal_moral_status__abolitionist_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__abolitionist_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(anim_be_t10, animal_moral_status__abolitionist_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(anim_be_t20, animal_moral_status__abolitionist_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(anim_be_t30, animal_moral_status__abolitionist_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__abolitionist_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(anim_be_t50, animal_moral_status__abolitionist_reading, base_extractiveness, 50, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__abolitionist_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(anim_su_t10, animal_moral_status__abolitionist_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(anim_su_t20, animal_moral_status__abolitionist_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(anim_su_t30, animal_moral_status__abolitionist_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__abolitionist_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(anim_su_t50, animal_moral_status__abolitionist_reading, suppression_requirement, 50, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__welfare_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the animal_moral_status kernel, decomposed per the Îµ-invariance principle. The property_reading and welfare_reading are sibling constraints with different Îµ values and stakeholder structures. This reading (abolitionist) carries the highest Îµ because it recognizes all animal use as extractive victimization with no coordinating benefit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
